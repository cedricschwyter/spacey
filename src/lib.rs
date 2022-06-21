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

#[derive(Debug, Clone)]
enum ParseErrorKind {
    InvalidToken(usize, Vec<u8>),
    UnexpectedToken(usize, u8, Vec<u8>),
}

impl Display for ParseErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug)]
#[allow(dead_code)]
struct ParseError {
    msg: String,
    kind: ParseErrorKind,
}

impl Error for ParseError {}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum ImpKind {
    Stack,
    Arithmetic,
    Heap,
    Flow,
    IO,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum CommandKind {
    PushStack,
    DuplicateStack,
    CopyNthStack,
    SwapStack,
    DiscardStack,
    SlideNStack,
    Add,
    Subtract,
    Multiply,
    IntegerDivision,
    Modulo,
    StoreHeap,
    RetrieveHeap,
    Mark,
    Call,
    Jump,
    JumpZero,
    JumpNegative,
    Return,
    Exit,
    OutCharacter,
    OutInteger,
    ReadCharacter,
    ReadInteger,
}

impl CommandKind {
    fn param_kind(&self) -> Option<ParamKind> {
        match self {
            CommandKind::PushStack | CommandKind::CopyNthStack | CommandKind::SlideNStack => {
                Some(ParamKind::Number(0))
            }
            CommandKind::Mark
            | CommandKind::Call
            | CommandKind::Jump
            | CommandKind::JumpZero
            | CommandKind::JumpNegative => Some(ParamKind::Label("".to_string())),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq)]
enum ParamKind {
    Number(i32),
    Label(String),
}

#[derive(Debug, PartialEq)]
pub struct Instruction {
    imp: ImpKind,
    cmd: CommandKind,
    param: Option<ParamKind>,
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

    fn throw<T>(&self, kind: ParseErrorKind) -> Result<T, Box<dyn Error>> {
        let msg = match &kind {
            ParseErrorKind::UnexpectedToken(pos, token, tokens) => format!(
                "unexpected token at position {}, expected one of {:?}, but got {}",
                pos,
                tokens.iter().map(|b| *b as char).collect::<Vec<_>>(),
                *token as char
            ),
            ParseErrorKind::InvalidToken(pos, tokens) => format!(
                "unexpected token at position {}, expected one of {:?}",
                pos,
                tokens.iter().map(|b| *b as char).collect::<Vec<_>>(),
            ),
        };
        Err(Box::new(ParseError { msg, kind }))
    }

    fn next(&mut self) -> Option<u8> {
        if self.index < self.source.len() - 1 {
            self.index += 1;
            return Some(self.source[self.index - 1]);
        }
        None
    }

    fn imp(&mut self) -> Option<Result<ImpKind, Box<dyn Error>>> {
        let val = self.next()?;
        match val {
            SPACE => Some(Ok(ImpKind::Stack)),
            TAB => {
                if let Some(val) = self.next() {
                    match val {
                        SPACE => Some(Ok(ImpKind::Arithmetic)),
                        TAB => Some(Ok(ImpKind::Heap)),
                        LINE_FEED => Some(Ok(ImpKind::IO)),
                        _ => Some(self.throw(ParseErrorKind::UnexpectedToken(
                            self.index,
                            val,
                            vec![SPACE, TAB, LINE_FEED],
                        ))),
                    }
                } else {
                    Some(self.throw(ParseErrorKind::UnexpectedToken(
                        self.index,
                        val,
                        vec![SPACE, TAB, LINE_FEED],
                    )))
                }
            }
            LINE_FEED => Some(Ok(ImpKind::Flow)),
            _ => Some(self.throw(ParseErrorKind::UnexpectedToken(
                self.index,
                val,
                vec![SPACE, TAB, LINE_FEED],
            ))),
        }
    }

    fn stack(&mut self) -> Option<Result<CommandKind, Box<dyn Error>>> {
        let val = self.next()?;
        match val {
            SPACE => Some(Ok(CommandKind::PushStack)),
            TAB => {
                if let Some(val) = self.next() {
                    return match val {
                        SPACE => Some(Ok(CommandKind::CopyNthStack)),
                        LINE_FEED => Some(Ok(CommandKind::SlideNStack)),
                        _ => Some(self.throw(ParseErrorKind::UnexpectedToken(
                            self.index,
                            val,
                            vec![SPACE, LINE_FEED],
                        ))),
                    };
                }
                Some(self.throw(ParseErrorKind::InvalidToken(
                    self.index,
                    vec![SPACE, LINE_FEED],
                )))
            }
            LINE_FEED => {
                if let Some(val) = self.next() {
                    return match val {
                        SPACE => Some(Ok(CommandKind::DuplicateStack)),
                        TAB => Some(Ok(CommandKind::SwapStack)),
                        LINE_FEED => Some(Ok(CommandKind::DiscardStack)),
                        _ => Some(self.throw(ParseErrorKind::UnexpectedToken(
                            self.index,
                            val,
                            vec![SPACE, TAB, LINE_FEED],
                        ))),
                    };
                }
                Some(self.throw(ParseErrorKind::InvalidToken(
                    self.index,
                    vec![SPACE, TAB, LINE_FEED],
                )))
            }
            _ => Some(self.throw(ParseErrorKind::UnexpectedToken(
                self.index,
                val,
                vec![SPACE, TAB, LINE_FEED],
            ))),
        }
    }

    fn arithmetic(&mut self) -> Option<Result<CommandKind, Box<dyn Error>>> {
        unimplemented!()
    }

    fn heap(&mut self) -> Option<Result<CommandKind, Box<dyn Error>>> {
        let val = self.next()?;
        match val {
            SPACE => Some(Ok(CommandKind::StoreHeap)),
            TAB => Some(Ok(CommandKind::RetrieveHeap)),
            _ => Some(self.throw(ParseErrorKind::InvalidToken(self.index, vec![SPACE, TAB]))),
        }
    }

    fn flow(&mut self) -> Option<Result<CommandKind, Box<dyn Error>>> {
        let val = self.next()?;
        match val {
            LINE_FEED => {
                if let Some(val) = self.next() {
                    return match val {
                        LINE_FEED => Some(Ok(CommandKind::Exit)),
                        _ => unimplemented!(),
                    };
                }
                unimplemented!()
            }
            _ => unimplemented!(),
        }
    }

    fn io(&mut self) -> Option<Result<CommandKind, Box<dyn Error>>> {
        unimplemented!()
    }

    fn cmd(&mut self, imp: ImpKind) -> Option<Result<CommandKind, Box<dyn Error>>> {
        match imp {
            ImpKind::Stack => Some(self.stack()?),
            ImpKind::Arithmetic => Some(self.arithmetic()?),
            ImpKind::Heap => Some(self.heap()?),
            ImpKind::Flow => Some(self.flow()?),
            ImpKind::IO => Some(self.io()?),
        }
    }

    fn number(&mut self, sign: i32) -> Option<Result<ParamKind, Box<dyn Error>>> {
        let mut places = Vec::new();
        let mut failure = None;
        while let Some(val) = self.next() {
            places.push(match val {
                SPACE => 0,
                TAB => 1,
                LINE_FEED => {
                    break;
                }
                _ => {
                    failure = Some(self.throw(ParseErrorKind::UnexpectedToken(
                        self.index,
                        val,
                        vec![SPACE, TAB],
                    )));
                    break;
                }
            });
        }
        if failure.is_some() {
            return failure;
        }
        let mut res = 0;
        let mut place = 0;
        while let Some(val) = places.pop() {
            res += val * (2_i32.pow(place));
            place += 1;
        }

        Some(Ok(ParamKind::Number(sign * res)))
    }

    fn param(&mut self, kind: ParamKind) -> Option<Result<ParamKind, Box<dyn Error>>> {
        match kind {
            ParamKind::Label(_) => Some(Ok(ParamKind::Label("".to_string()))),
            ParamKind::Number(_) => {
                if let Some(val) = self.next() {
                    return match val {
                        SPACE => self.number(1),
                        TAB => self.number(-1),
                        _ => Some(self.throw(ParseErrorKind::UnexpectedToken(
                            self.index,
                            val,
                            vec![SPACE, TAB],
                        ))),
                    };
                }
                Some(self.throw(ParseErrorKind::InvalidToken(self.index, vec![SPACE, TAB])))
            }
        }
    }

    fn instruction(&mut self) -> Option<Result<Instruction, Box<dyn Error>>> {
        let imp = self.imp()?;
        if let Ok(imp) = imp {
            let cmd = self.cmd(imp)?;
            if let Ok(cmd) = cmd {
                let mut param = None;
                let kind = cmd.param_kind();
                if let Some(kind) = kind {
                    let par = self.param(kind)?;
                    if let Ok(par) = par {
                        param = Some(par);
                    } else if let Err(err) = par {
                        return Some(Err(err));
                    }
                };
                let instr = Instruction { imp, cmd, param };

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

impl Iterator for Interpreter {
    type Item = Result<Instruction, Box<dyn Error>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.parser.instruction()
    }
}

impl InterpreterContext {
    pub fn exec(&self, instr: Instruction) -> Result<(), Box<dyn Error>> {
        todo!("implement execution logic here");
    }
}

#[cfg(test)]
mod tests {
    use crate::ParamKind;

    use super::{CommandKind, ImpKind, Instruction, Interpreter};
    use std::error::Error;

    fn test_parse(
        interpreter: Interpreter,
        results: Vec<Instruction>,
    ) -> Result<(), Box<dyn Error>> {
        for (i, instr) in interpreter.enumerate() {
            if i == results.len() {
                break;
            }
            if let Ok(instr) = instr {
                assert_eq!(instr, results[i]);
            } else if let Err(err) = instr {
                return Err(err);
            }
        }

        Ok(())
    }

    #[test]
    fn stack() -> Result<(), Box<dyn Error>> {
        let interpreter = Interpreter::new("ws/stack.ws")?;
        let results = vec![
            Instruction {
                imp: ImpKind::Stack,
                cmd: CommandKind::PushStack,
                param: Some(ParamKind::Number(64)),
            },
            Instruction {
                imp: ImpKind::Stack,
                cmd: CommandKind::DuplicateStack,
                param: None,
            },
            Instruction {
                imp: ImpKind::Stack,
                cmd: CommandKind::CopyNthStack,
                param: Some(ParamKind::Number(64)),
            },
            Instruction {
                imp: ImpKind::Stack,
                cmd: CommandKind::SwapStack,
                param: None,
            },
            Instruction {
                imp: ImpKind::Stack,
                cmd: CommandKind::DiscardStack,
                param: None,
            },
            Instruction {
                imp: ImpKind::Stack,
                cmd: CommandKind::SlideNStack,
                param: Some(ParamKind::Number(64)),
            },
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::Exit,
                param: None,
            },
        ];

        test_parse(interpreter, results)
    }

    #[test]
    fn heap() -> Result<(), Box<dyn Error>> {
        let interpreter = Interpreter::new("ws/heap.ws")?;
        let results = vec![
            Instruction {
                imp: ImpKind::Heap,
                cmd: CommandKind::StoreHeap,
                param: None,
            },
            Instruction {
                imp: ImpKind::Heap,
                cmd: CommandKind::RetrieveHeap,
                param: None,
            },
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::Exit,
                param: None,
            },
        ];

        test_parse(interpreter, results)
    }
}
