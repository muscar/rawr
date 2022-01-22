use core::panic;
use regex::Regex;
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
struct Program {
    body: Vec<(Option<Pattern>, Action)>,
}

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
    Print(Vec<Expression>),
}

#[derive(Debug, Clone)]
enum Expression {
    IntLit(i64),
    Field(Box<Expression>),
}

// Scan & parse

#[derive(Debug, PartialEq, Eq)]
enum TokenKind {
    IntLit,
    RegexLit,
    Dollar,
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
            Some('$') => {
                self.chars.next();
                Some(Token::new(TokenKind::Dollar, "$".to_string()))
            }
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
            Some(c) if c.is_digit(10) => {
                let mut lexeme = String::new();
                while matches!(self.chars.peek(), Some(c) if c.is_digit(10)) {
                    lexeme.push(self.chars.next().unwrap());
                }
                Some(Token::new(TokenKind::IntLit, lexeme))
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
                            body: vec![Statement::Print(vec![Expression::Field(Box::new(
                                Expression::IntLit(0),
                            ))])],
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
        Statement::Print(vec![self.parse_expression()])
    }

    fn parse_expression(&mut self) -> Expression {
        match self
            .tokens
            .next()
            .expect("syntactic error: unexpected end of file")
        {
            Token {
                kind: TokenKind::IntLit,
                text,
            } => Expression::IntLit(text.parse().unwrap()),
            Token {
                kind: TokenKind::Dollar,
                ..
            } => {
                let idx = self.parse_expression();
                Expression::Field(Box::new(idx))
            }
            tok => panic!("syntactic error: expecting expression near {:?}", tok.kind),
        }
    }
}

// Compile

struct CodeGen {
    code: Vec<Op>,
}

impl CodeGen {
    fn new() -> Self {
        Self { code: vec![] }
    }

    fn emit(&mut self, op: Op) {
        self.code.push(op);
    }
}

fn compile(gen: &mut CodeGen, source: &str) -> Result<(), Box<dyn Error>> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);
    let node = parser.parse()?;
    compile_program(gen, &node);
    Ok(())
}

fn compile_program(gen: &mut CodeGen, prog: &Program) {
    for pattern_action in &prog.body {
        compile_pattern_action(gen, pattern_action);
    }
}

fn compile_pattern_action(gen: &mut CodeGen, pattern_action: &(Option<Pattern>, Action)) {
    if let Some(pattern) = &pattern_action.0 {
        compile_pattern(gen, pattern);
    }
    compile_action(gen, &pattern_action.1);
}

fn compile_pattern(gen: &mut CodeGen, pattern: &Pattern) {
    match pattern {
        Pattern::Re(re) => {
            gen.emit(ld_rec());
            gen.emit(match_re(re.clone()));
        }
    }
}

fn compile_action(gen: &mut CodeGen, action: &Action) {
    for stmt in &action.body {
        compile_statement(gen, stmt);
    }
}

fn compile_statement(gen: &mut CodeGen, stmt: &Statement) {
    match stmt {
        Statement::Print(args) => {
            for expr in args.iter().rev() {
                compile_expression(gen, expr);
            }
            gen.emit(print(args.len()));
        }
    }
}

fn compile_expression(gen: &mut CodeGen, expr: &Expression) {
    match expr {
        Expression::IntLit(n) => gen.emit(ldc_i8(*n)),
        Expression::Field(idx) => {
            compile_expression(gen, idx);
            gen.emit(ld_fld())
        }
    }
}

// VM

type Op = Box<dyn Fn(&mut State, &Record) -> i64>;

fn ldc_i8(n: i64) -> Op {
    Box::new(move |state: &mut State, _: &Record| {
        // println!("ldc_i8 {:?}", n);
        state.stack.push(Value::Int(n));
        1
    })
}

fn ld_rec() -> Op {
    Box::new(move |state: &mut State, record: &Record| {
        // println!("ld_rec");
        state.stack.push(Value::Str(record.text.clone()));
        1
    })
}

fn ld_fld() -> Op {
    Box::new(|state: &mut State, record: &Record| {
        match state.stack.pop().map(|v| v.int_val()).unwrap() {
            0 => {
                // println!("ld_rec");
                state.stack.push(Value::Str(record.text.clone()));
                1
            }
            idx => {
                // println!("ldc_fld {:?}", idx);
                state
                    .stack
                    .push(Value::Str(record.get_field((idx - 1) as usize).text));
                1
            }
        }
    })
}

fn match_re(re: Regex) -> Op {
    Box::new(move |state: &mut State, _: &Record| {
        let v = state.stack.pop().map(|v| v.str_val()).unwrap();
        // println!("match_re {:?} {:?}", re, v);
        let v = if re.is_match(&v) { 1 } else { 0 };
        // state.stack.push(Value::Int(v));
        v
    })
}

fn print(argc: usize) -> Op {
    Box::new(move |state: &mut State, _: &Record| {
        // println!("print/{:?}", argc);
        for _ in 0..argc {
            let x = state.stack.pop().unwrap();
            state
                .output
                .write_fmt(format_args!("{}", x.str_val()))
                .unwrap();
            state.output.write_all(&[b'\n']).unwrap();
        }
        1
    })
}

#[derive(Debug, Clone)]
enum Value {
    Int(i64),
    Str(String),
}

impl Value {
    fn int_val(&self) -> i64 {
        match self {
            Value::Int(n) => *n,
            Value::Str(s) => s.parse().unwrap(),
        }
    }

    fn str_val(&self) -> String {
        match self {
            Value::Int(n) => n.to_string(),
            Value::Str(s) => s.clone(),
        }
    }
}

struct State {
    output: BufWriter<File>,
    stack: Vec<Value>,
}

impl State {
    fn new(out: BufWriter<File>) -> Self {
        Self {
            output: out,
            stack: vec![],
        }
    }
}

struct RecordReader {
    reader: BufReader<File>,
}

impl RecordReader {
    fn new(reader: BufReader<File>) -> Self {
        Self { reader }
    }

    fn records(self) -> impl Iterator<Item = Record> {
        self.reader
            .split(b'\n')
            .map(|bytes| Record::from_utf8(bytes.unwrap()))
    }
}

struct Record {
    text: String,
}

impl Record {
    fn from_utf8(bytes: Vec<u8>) -> Self {
        Self {
            text: String::from_utf8(bytes).unwrap(),
        }
    }

    fn get_field(&self, idx: usize) -> Field {
        Field::new(self.text.split(' ').nth(idx).unwrap().to_string())
    }
}

struct Field {
    text: String,
}

impl Field {
    fn new(text: String) -> Self {
        Self { text }
    }
}

enum Input {
    Stdin,
    File(String), // TODO: Path, PathBuf?
}

impl Input {
    fn get_reader(&self) -> RecordReader {
        let f = match self {
            Input::Stdin => unsafe { File::from_raw_fd(0) },
            Input::File(path) => File::open(path).unwrap(),
        };
        RecordReader::new(BufReader::new(f))
    }
}

fn run_program(inputs: Vec<Input>, state: &mut State, prog: Vec<Op>) {
    for (fnr, input) in inputs.iter().enumerate() {
        // println!("FNR={:?}", fnr);
        let reader = input.get_reader();
        for (nr, record) in reader.records().enumerate() {
            // println!("NR={:?}", nr);
            let mut ip = 0;
            while ip < prog.len() {
                // println!("ip={}", ip);
                // println!("-> {:?}", state.stack);
                let step = prog[ip](state, &record);
                // println!("<- [{step}] {:?}", state.stack);
                if step == 0 {
                    break;
                }
                ip = (ip as i64 + step) as usize;
            }
        }
    }
}

// Program

fn rawr_main(mut args: impl Iterator<Item = String>) -> Result<(), Box<dyn Error>> {
    let prog = match args.next() {
        Some(pat) => pat,
        None => panic!("usage: rawr PROGRAM FILE..."),
    };
    let mut paths = args.collect::<Vec<_>>();
    if paths.is_empty() {
        paths.push("-".into());
    }
    let mut gen = CodeGen::new();
    compile(&mut gen, &prog)?;

    let inputs = paths
        .iter()
        .map(|p| {
            if p == "-" {
                Input::Stdin
            } else {
                Input::File(p.clone())
            }
        })
        .collect::<Vec<_>>();
    let stdout = AsRawFd::as_raw_fd(&io::stdout());
    let stdout: File = unsafe { FromRawFd::from_raw_fd(stdout) };
    let output = std::io::BufWriter::with_capacity(32 * 1024, stdout);
    let mut state = State::new(output);

    run_program(inputs, &mut state, gen.code);

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
