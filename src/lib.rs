use std::error::Error;
use std::fs::File;
use std::io::{BufRead, BufReader, Lines};

#[derive(Debug)]
struct Parser {
    source: Lines<BufReader<File>>,
}

#[derive(Debug)]
struct InterpreterContext {
    labels: Vec<String>,
}

#[derive(Debug)]
pub struct Interpreter {
    parser: Parser,
    interpreter: InterpreterContext,
}

#[derive(Debug)]
enum TokenKind {
    Space,
    Tab,
    Linefeed,
}

#[derive(Debug)]
struct Token {
    kind: TokenKind,
    line: u32,
    col: u32,
}

#[derive(Debug)]
enum ImpKind {
    Stack,
    Arithmetic,
    Heap,
    Flow,
    IO,
}

#[derive(Debug)]
enum CommandKind {}

#[derive(Debug)]
enum ParamType {
    Number(u32),
    Label(String),
}

#[derive(Debug)]
pub struct Instruction {
    imp: ImpKind,
    cmd: CommandKind,
    param: Option<ParamType>,
}

impl InterpreterContext {
    fn new() -> InterpreterContext {
        let labels = vec![];

        InterpreterContext { labels }
    }
}

impl Interpreter {
    pub fn new(file_name: &str) -> Result<Interpreter, Box<dyn Error>> {
        let parser = Parser::new(file_name)?;
        let interpreter = InterpreterContext::new();

        Ok(Interpreter {
            parser,
            interpreter,
        })
    }

    pub fn run(self) -> Result<(), Box<dyn Error>> {
        for instr in self.parser {
            match self.interpreter.exec(instr) {
                Ok(_) => continue,
                Err(err) => return Err(err),
            };
        }

        Ok(())
    }
}

impl Parser {
    fn new(file_name: &str) -> Result<Parser, Box<dyn Error>> {
        let file = File::open(&file_name)?;
        let source = BufReader::new(file).lines();

        Ok(Parser { source })
    }

    fn instruction(&mut self) -> Option<Instruction> {
        todo!("implement parsing logic here");
    }
}

impl Iterator for Parser {
    type Item = Instruction;

    fn next(&mut self) -> Option<Self::Item> {
        self.instruction()
    }
}

impl InterpreterContext {
    pub fn exec(&self, instr: Instruction) -> Result<(), Box<dyn Error>> {
        todo!("implement execution logic here");
    }
}
