use memmap::Mmap;
use std::error::Error;
use std::fmt::Display;
use std::fs::File;

const SPACE: u8 = b' ';
const TAB: u8 = b'\t';
const LINE_FEED: u8 = b'\n';

#[derive(Debug)]
struct Parser {
    source: Mmap,
    index: usize,
}

struct InterpreterContext {
    labels: Vec<String>,
}

pub struct Interpreter {
    parser: Parser,
    interpreter: InterpreterContext,
}

#[derive(Debug)]
enum ParseErrorKind {
    UnexpectedToken,
}

#[derive(Debug)]
struct ParseError {
    msg: String,
    kind: ParseErrorKind,
}

impl Error for ParseError {}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
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
struct Instruction {
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
            dbg!(instr?);
        }

        Ok(())
    }
}

impl Parser {
    fn new(file_name: &str) -> Result<Parser, Box<dyn Error>> {
        let file = File::open(&file_name)?;
        let source = unsafe { Mmap::map(&file)? };
        let index = 0;

        Ok(Parser { source, index })
    }

    fn throw<T>(&self, msg: String, kind: ParseErrorKind) -> Result<T, Box<dyn Error>> {
        Err(Box::new(ParseError { msg, kind }))
    }

    fn current(&self) -> u8 {
        self.source[self.index]
    }

    fn next(&mut self) -> Option<u8> {
        if self.index < self.source.len() - 1 {
            return Some(self.source[self.index + 1]);
        }
        None
    }

    fn accept(&mut self, sym: u8) -> Option<u8> {
        if self.current() == sym {
            return self.next();
        }
        None
    }

    fn expect(&mut self, sym: u8) -> Result<(), Box<dyn Error>> {
        if self.accept(sym).is_some() {
            return Ok(());
        }
        self.throw(
            format!(
                "expected {}, got {} at position {}",
                sym,
                self.current(),
                self.index
            ),
            ParseErrorKind::UnexpectedToken,
        )
    }

    fn imp(&mut self) -> Option<Result<ImpKind, Box<dyn Error>>> {
        if self.accept(SPACE).is_some() {
            return Some(Ok(ImpKind::Stack));
        }
        if self.accept(TAB).is_some() {
            if let Some(val) = self.next() {
                return match val {
                    SPACE => Some(Ok(ImpKind::Arithmetic)),
                    TAB => Some(Ok(ImpKind::Heap)),
                    LINE_FEED => Some(Ok(ImpKind::IO)),
                    _ => Some(self.throw(
                        format!(
                            "expected one of SPACE, TAB or LINE_FEED, but got {} at position {}",
                            val, self.index
                        ),
                        ParseErrorKind::UnexpectedToken,
                    )),
                };
            } else {
                return None;
            }
        }
        if self.accept(LINE_FEED).is_some() {
            return Some(Ok(ImpKind::Flow));
        }

        Some(self.throw(
            format!(
                "no valid token found at position {}, expected one of SPACE, TAB or LINE_FEED",
                self.index
            ),
            ParseErrorKind::UnexpectedToken,
        ))
    }

    fn cmd(&mut self, imp: &ImpKind) -> Option<Result<CommandKind, Box<dyn Error>>> {
        unimplemented!();
    }

    fn param(&mut self, cmd: &CommandKind) -> Option<Result<ParamType, Box<dyn Error>>> {
        unimplemented!();
    }

    fn instruction(&mut self) -> Option<Result<Instruction, Box<dyn Error>>> {
        let imp = self.imp()?;
        if let Ok(imp) = imp {
            let cmd = self.cmd(&imp)?;
            if let Ok(cmd) = cmd {
                // TODO: add param for instructions with parameter
                let param = self.param(&cmd);
                let instr = Instruction {
                    imp,
                    cmd,
                    param: None,
                };

                return Some(Ok(instr));
            } else if let Err(err) = cmd {
                return Some(Err(err));
            }
        } else if let Err(err) = imp {
            return Some(Err(err));
        }

        None
    }
}

impl Iterator for Parser {
    type Item = Result<Instruction, Box<dyn Error>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.instruction()
    }
}

impl InterpreterContext {
    pub fn exec(&self, instr: Instruction) -> Result<(), Box<dyn Error>> {
        todo!("implement execution logic here");
    }
}
