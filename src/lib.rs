use std::error::Error;
use std::fs::File;
use std::io::{BufRead, BufReader, Lines};

#[derive(Debug)]
struct Parser {
    source: Lines<BufReader<File>>,
}

#[derive(Debug)]
pub struct Interpreter<'a> {
    file_name: &'a str,
    labels: Vec<String>,
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

impl Interpreter<'_> {
    pub fn new(file_name: &str) -> Interpreter {
        let labels = vec![];

        Interpreter { file_name, labels }
    }

    pub fn run(self) -> Result<(), Box<dyn Error>> {
        let parser = Parser::new(&self.file_name)?;
        self.run_all(parser)?;

        Ok(())
    }

    fn run_all(self, parser: Parser) -> Result<(), Box<dyn Error>> {
        for instr in parser {
            match self.exec(instr) {
                Ok(_) => continue,
                Err(err) => return Err(err),
            };
        }

        Ok(())
    }

    pub fn exec(&self, instr: Instruction) -> Result<(), Box<dyn Error>> {
        todo!("implement execution logic here");
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

impl<'a> Iterator for Parser {
    type Item = Instruction;

    fn next(&mut self) -> Option<Self::Item> {
        self.instruction()
    }
}
