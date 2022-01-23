use core::panic;
use pprof::protos::Message;
use regex::bytes::Regex;
use std::{
    error::Error,
    fs::File,
    io::{self, BufRead, BufReader, BufWriter, Write},
    iter::Peekable,
    ops::Index,
    os::unix::prelude::{AsRawFd, FromRawFd},
    str::{self, Chars},
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
        Expression::Field(expr) => match &**expr {
            Expression::IntLit(idx) => gen.emit(ld_fld(*idx as usize)),
            Expression::Field(expr) => {
                compile_expression(gen, expr);
                gen.emit(ld_fld_dynamic())
            }
        },
    }
}

// VM

type Op = Box<dyn Fn(&mut Channels, &mut State) -> i64>;

fn ldc_i8(n: i64) -> Op {
    Box::new(move |_: &mut Channels, state: &mut State| {
        // println!("ldc_i8 {:?}", n);
        state.stack.push(Value::Int(n));
        1
    })
}

fn ld_rec() -> Op {
    Box::new(move |_: &mut Channels, state: &mut State| {
        // println!("ld_rec");
        state
            .stack
            .push(Value::Str(StrPoolIdx(0), state.rec_bytes_len));
        1
    })
}

fn ld_fld(idx: usize) -> Op {
    Box::new(move |_: &mut Channels, state: &mut State| {
        let field = state.get_field(idx);
        state.stack.push(field);
        1
    })
}

fn ld_fld_dynamic() -> Op {
    Box::new(|_: &mut Channels, state: &mut State| {
        let idx = match state.stack.pop().unwrap() {
            Value::Int(n) => n,
            Value::Str(idx, len) => unsafe { str::from_utf8_unchecked(state.get_string(idx, len)) }
                .parse::<i64>()
                .unwrap(),
        };
        let field = state.get_field(idx as usize);
        state.stack.push(field);
        1
    })
}

fn match_re(re: Regex) -> Op {
    Box::new(move |_: &mut Channels, state: &mut State| {
        let bytes = match state.stack.pop().unwrap() {
            Value::Int(_) => panic!("not a string"),
            Value::Str(idx, len) => state.get_string(idx, len),
        };
        // println!("match_re {:?} {:?}", re, v);
        let v = if re.is_match(bytes) { 1 } else { 0 };
        // state.stack.push(Value::Int(v));
        v
    })
}

fn print(argc: usize) -> Op {
    Box::new(move |channels: &mut Channels, state: &mut State| {
        // println!("print/{:?}", argc);
        for _ in 0..argc {
            match state.stack.pop().unwrap() {
                Value::Int(n) => {
                    let bytes = &i64::to_ne_bytes(n);
                    channels.output.write_all(bytes).unwrap();
                }
                Value::Str(idx, len) => {
                    channels
                        .output
                        .write_all(state.get_string(idx, len))
                        .unwrap();
                }
            }
            channels.output.write_all(&[b'\n']).unwrap();
        }
        1
    })
}

struct StrPoolIdx(usize);

enum Value {
    Int(i64),
    Str(StrPoolIdx, usize),
}

impl Value {
    // fn int_val(&self, str_pool: Vec<u8>) -> i64 {
    //     match self {
    //         Value::Int(n) => *n,
    //         Value::Str(idx, len) => {
    //             let bytes = &str_pool[idx.0..idx.0 + len];
    //             i64::from_str_radix(unsafe { str::from_utf8_unchecked(bytes) }, 10).unwrap()
    //         }
    //     }
    // }

    // fn str_val(&self, str_pool: Vec<u8>) -> &[u8] {
    //     match self {
    //         Value::Int(n) => &i64::to_ne_bytes(*n).to_vec(), // XXX
    //         Value::Str(idx, len) => &str_pool[idx.0..idx.0 + len],
    //     }
    // }
}

struct Channels {
    output: BufWriter<File>,
}

impl Channels {
    fn new(output: BufWriter<File>) -> Self {
        Self { output }
    }
}

struct State {
    stack: Vec<Value>,
    str_pool: Vec<u8>,
    rec_bytes_len: usize,
    field_offs: Vec<usize>,
}

impl State {
    fn new() -> Self {
        Self {
            stack: vec![],
            str_pool: vec![],
            rec_bytes_len: 0,
            field_offs: vec![],
        }
    }

    fn set_record(&mut self, bytes: &[u8]) {
        self.str_pool.clear();
        self.str_pool.extend_from_slice(bytes);
        self.rec_bytes_len = bytes.len();
        self.field_offs.clear();
    }

    fn intern_string(&mut self, s: &[u8]) -> StrPoolIdx {
        let idx = StrPoolIdx(self.str_pool.len());
        self.str_pool.extend_from_slice(s);
        idx
    }

    fn get_string(&self, idx: StrPoolIdx, len: usize) -> &[u8] {
        &self.str_pool[idx.0..idx.0 + len]
    }

    fn get_field(&mut self, idx: usize) -> Value {
        if idx == 0 {
            return Value::Str(StrPoolIdx(0), self.rec_bytes_len);
        }
        if self.field_offs.is_empty() {
            self.field_offs.push(0);
            for (off, b) in self.str_pool.iter().enumerate() {
                if *b == b' ' {
                    self.field_offs.push(off + 1);
                }
            }
            self.field_offs.push(self.rec_bytes_len);
        }
        Value::Str(
            StrPoolIdx(self.field_offs[idx - 1]),
            self.field_offs[idx] - self.field_offs[idx - 1],
        )
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
            .map(|bytes| Record::new(bytes.unwrap()))
    }
}

struct Record {
    bytes: Vec<u8>,
}

impl Record {
    fn new(bytes: Vec<u8>) -> Self {
        Self { bytes }
    }

    fn get_field(&self, idx: usize) -> Field {
        Field::new(self.bytes.split(|b| *b == b' ').nth(idx).unwrap())
    }
}

struct Field<'a> {
    bytes: &'a [u8],
}

impl<'a> Field<'a> {
    fn new(bytes: &'a [u8]) -> Self {
        Self { bytes }
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

fn run_program(inputs: Vec<Input>, mut channels: Channels, prog: Vec<Op>) {
    let mut state = State::new();
    for (fnr, input) in inputs.iter().enumerate() {
        // println!("FNR={:?}", fnr);
        let reader = input.get_reader();
        for (nr, record) in reader.records().enumerate() {
            // println!("NR={:?}", nr);
            state.set_record(&record.bytes);
            let mut ip = 0;
            while ip < prog.len() {
                // println!("ip={}", ip);
                // println!("-> {:?}", state.stack);
                let step = prog[ip](&mut channels, &mut state);
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

fn rawr_main(opts: &Options) -> Result<(), Box<dyn Error>> {
    // let guard = pprof::ProfilerGuard::new(100).unwrap();

    let mut gen = CodeGen::new();
    compile(&mut gen, &opts.prog)?;

    let inputs = opts
        .paths
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
    let channels = Channels::new(output);

    run_program(inputs, channels, gen.code);

    // if let Ok(report) = guard.report().build() {
    //     let mut file = File::create("cpu.prof").unwrap();
    //     let profile = report.pprof().unwrap();

    //     let mut content = Vec::new();
    //     profile.encode(&mut content).unwrap();
    //     file.write_all(&content).unwrap();

    //     // println!("report: {:?}", &report);
    // };

    Ok(())
}

#[derive(Debug)]
struct Options {
    cpu_profile_file_path: Option<String>,
    mem_profile_file_path: Option<String>,
    prog: String,
    paths: Vec<String>,
}

fn parse_args(args: impl Iterator<Item = String>) -> Result<Options, String> {
    let mut result = Options {
        cpu_profile_file_path: None,
        mem_profile_file_path: None,
        prog: "".to_string(),
        paths: vec![],
    };
    let (options, rest_args): (Vec<String>, Vec<String>) = std::env::args()
        .skip(1)
        .partition(|arg| arg.starts_with("--"));
    for opt in options {
        let parts = opt.split('=').collect::<Vec<_>>();
        match parts[0] {
            "--cpuprofile" => result.cpu_profile_file_path = Some(parts[1].to_string()),
            "--memprofile" => result.mem_profile_file_path = Some(parts[1].to_string()),
            _ => return Err(format!("unrecognised option: {}", opt)),
        }
    }
    let mut rest_args_it = rest_args.iter();
    result.prog = match rest_args_it.next() {
        Some(pat) => pat.clone(),
        None => panic!("usage: rawr PROGRAM FILE..."),
    };
    result.paths = rest_args_it.cloned().collect::<Vec<_>>();
    if result.paths.is_empty() {
        result.paths.push("-".into());
    }
    Ok(result)
}

fn main() {
    // let args = std::env::args().skip(1);
    let opts = parse_args(std::env::args()).unwrap();
    // println!("{:?}", opts);
    let result = rawr_main(&opts);
    if let Err(err) = result {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}
