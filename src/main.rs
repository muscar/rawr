use regex::bytes::Regex;
use std::{
    error::Error,
    fs::File,
    io::{self, BufRead, BufReader, BufWriter, Write},
    iter::Peekable,
    os::unix::prelude::{AsRawFd, FromRawFd},
    str::Chars,
};

// AST

#[derive(Debug)]
enum Pattern {
    Re(Regex),
}

#[derive(Debug)]
struct Action {
    body: Vec<Statement>,
}

#[derive(Debug)]
enum Statement {
    Print,
}

#[derive(Debug)]
struct Program {
    body: Vec<(Option<Pattern>, Action)>,
}

// Scan & parse

#[derive(Debug, PartialEq, Eq)]
enum TokenKind {
    RegexLit,
    LBrace,
    RBrace,
    Print,
}

struct Token {
    kind: TokenKind,
    text: String,
}

impl Token {
    fn new(kind: TokenKind, text: String) -> Self {
        Self { kind, text }
    }
}

struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars().peekable(),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while matches!(self.chars.peek(), Some(c) if c.is_whitespace()) {
            self.chars.next();
        }
        match self.chars.peek() {
            None => None,
            Some('{') => {
                self.chars.next();
                Some(Token::new(TokenKind::LBrace, "{".to_string()))
            }
            Some('}') => {
                self.chars.next();
                Some(Token::new(TokenKind::RBrace, "}".to_string()))
            }
            Some('/') => {
                self.chars.next();
                let mut lexeme = String::new();
                while matches!(self.chars.peek(), Some(c) if *c != '/') {
                    lexeme.push(self.chars.next().unwrap());
                }
                if self.chars.peek() != Some(&'/') {
                    panic!("lexical error: missing `/' after regex literal");
                }
                self.chars.next();
                Some(Token::new(TokenKind::RegexLit, lexeme))
            }
            Some(c) if c.is_alphabetic() => {
                let mut lexeme = String::new();
                while matches!(self.chars.peek(), Some(c) if c.is_alphabetic()) {
                    lexeme.push(self.chars.next().unwrap());
                }
                if lexeme == "print" {
                    return Some(Token::new(TokenKind::Print, lexeme));
                }
                panic!("lexical error: unexpected token `{}'", lexeme);
            }
            Some(c) => panic!("lexical error: unexpected character `{}'", c),
        }
    }
}

struct Parser<'a> {
    tokens: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer<'a>) -> Self {
        Self {
            tokens: lexer.peekable(),
        }
    }

    fn expect(&mut self, expected: TokenKind) -> Token {
        match self.tokens.next() {
            Some(tok) if tok.kind == expected => tok,
            Some(tok) => panic!(
                "syntactic error: expecting {:?}, got {:?}",
                expected, tok.kind
            ),
            None => panic!("syntactic error: expecting {:?}, got end of file", expected),
        }
    }

    fn parse(&mut self) -> Result<Program, Box<dyn Error>> {
        match self.tokens.peek() {
            Some(tok) if tok.kind == TokenKind::LBrace => Ok(Program {
                body: vec![(None, self.parse_action())],
            }),
            _ => {
                let pattern = Some(self.parse_pattern()?);
                if matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::LBrace) {
                    return Ok(Program {
                        body: vec![(pattern, self.parse_action())],
                    });
                }
                Ok(Program {
                    body: vec![(
                        pattern,
                        Action {
                            body: vec![Statement::Print],
                        },
                    )],
                })
            }
        }
    }

    fn parse_pattern(&mut self) -> Result<Pattern, Box<dyn Error>> {
        let tok = self.expect(TokenKind::RegexLit);
        let re = Regex::new(&tok.text)?;
        Ok(Pattern::Re(re))
    }

    fn parse_action(&mut self) -> Action {
        self.expect(TokenKind::LBrace);
        let body = vec![self.parse_statement()];
        self.expect(TokenKind::RBrace);
        Action { body }
    }

    fn parse_statement(&mut self) -> Statement {
        self.expect(TokenKind::Print);
        Statement::Print
    }
}

// Compile

fn compile_program<R>(program: Program) -> impl Fn(&mut State, R)
where
    R: Iterator<Item = Record>,
{
    let mut compiled_pattern_actions = vec![];
    for (pattern, action) in program.body {
        compiled_pattern_actions.push(compile_pattern_action(pattern, action));
    }
    move |state: &mut State, records: R| {
        for r in records {
            for cpa in &compiled_pattern_actions {
                cpa(state, &r);
            }
        }
    }
}

fn compile_pattern_action(
    pattern: Option<Pattern>,
    action: Action,
) -> Box<dyn Fn(&mut State, &Record)> {
    let compiled_action = compile_action(action);
    match pattern {
        Some(p) => {
            let compiled_pattern = compile_pattern(p);
            Box::new(move |state: &mut State, record: &Record| {
                if compiled_pattern(state, record) {
                    compiled_action(state, record);
                }
            })
        }
        None => Box::new(move |state: &mut State, record: &Record| {
            compiled_action(state, record);
        }),
    }
}

fn compile_pattern(pattern: Pattern) -> impl Fn(&mut State, &Record) -> bool {
    match pattern {
        Pattern::Re(re) => move |_state: &mut State, record: &Record| re.is_match(record.bytes()),
    }
}

fn compile_action(action: Action) -> impl Fn(&mut State, &Record) {
    let mut compiled_actions = vec![];
    for s in action.body {
        compiled_actions.push(compile_statement(s));
    }
    move |state: &mut State, record: &Record| {
        for ca in &compiled_actions {
            ca(state, record);
        }
    }
}

fn compile_statement(statement: Statement) -> impl Fn(&mut State, &Record) {
    match statement {
        Statement::Print => |state: &mut State, record: &Record| {
            state.out.write(record.bytes()).unwrap();
            state.out.write(&[b'\n']).unwrap();
        },
    }
}

// Eval

struct State {
    out: BufWriter<File>,
}

struct Field<'a> {
    index: usize,
    bytes: &'a [u8],
}

impl<'a> Field<'a> {
    fn new(index: usize, bytes: &'a [u8]) -> Self {
        Self { index, bytes }
    }

    fn bytes(&self) -> &'a [u8] {
        self.bytes
    }
}

struct Record {
    bytes: Vec<u8>,
}

impl Record {
    fn new(bytes: Vec<u8>) -> Self {
        Self { bytes }
    }

    fn bytes(&self) -> &[u8] {
        &self.bytes
    }

    fn fields(&self) -> impl Iterator<Item = Field> {
        self.bytes
            .split(|c| *c == b' ')
            .enumerate()
            .map(|(idx, bytes)| Field::new(idx, bytes))
    }
}

struct RecordReader<R: BufRead> {
    reader: R,
}

impl<R: BufRead> RecordReader<R> {
    fn new(reader: R) -> Self {
        Self { reader }
    }

    fn records(self) -> impl Iterator<Item = Record> {
        self.reader
            .split(b'\n')
            .map(|bytes| Record::new(bytes.unwrap()))
    }
}

fn compile<R: Iterator<Item = Record>>(
    prog: &str,
) -> Result<impl Fn(&mut State, R), Box<dyn Error>> {
    let lexer = Lexer::new(&prog);
    let mut parser = Parser::new(lexer);
    let node = parser.parse()?;
    Ok(compile_program(node))
}

fn rawr_main(mut args: impl Iterator<Item = String>) -> Result<(), Box<dyn Error>> {
    let prog = match args.next() {
        Some(pat) => pat,
        None => panic!("usage: rawr PROGRAM FILE..."),
    };
    let mut paths = args.collect::<Vec<_>>();
    if paths.is_empty() {
        paths.push("-".into());
    }
    let compiled_prog = compile(&prog)?;

    let stdout = AsRawFd::as_raw_fd(&io::stdout());
    let stdout: File = unsafe { FromRawFd::from_raw_fd(stdout) };
    let out = std::io::BufWriter::with_capacity(32 * 1024, stdout);
    let mut state = State { out };

    for p in paths {
        let f = if p == "-" {
            unsafe { File::from_raw_fd(0) }
        } else {
            File::open(p)?
        };
        let rec_reader = RecordReader::new(BufReader::new(f));
        compiled_prog(&mut state, rec_reader.records());
    }

    Ok(())
}

fn main() {
    let args = std::env::args().skip(1);
    let result = rawr_main(args);
    if let Err(err) = result {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}
