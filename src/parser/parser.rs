#[cfg(not(target_arch = "wasm32"))]
use memmap::Mmap;
use std::error::Error;
use std::fmt::Display;
use std::fs::File;
use std::rc::Rc;
use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::JsValue;

pub const SPACE: u8 = b' ';
pub const TAB: u8 = b'\t';
pub const LINE_FEED: u8 = b'\n';

#[derive(Debug)]
pub(crate) enum ParseErrorKind {
    InvalidToken(usize, Vec<u8>, Vec<u8>),
    UnexpectedToken(usize, u8, Vec<u8>),
    FileOpenError(Box<dyn Error>),
    MemoryMapError(Box<dyn Error>),
}

impl ParseErrorKind {
    fn throw<T>(self) -> Result<T, ParseError> {
        let msg = match &self {
            ParseErrorKind::UnexpectedToken(pos, token, tokens) => format!(
                "unexpected token at position {}, expected one of {:?}, but got {}",
                pos,
                tokens.iter().map(|b| *b as char).collect::<Vec<_>>(),
                *token as char
            ),
            ParseErrorKind::InvalidToken(pos, tokens, rest) => format!(
                "unexpected token at position {}, expected one of {:?}. rest of file was: {:?}",
                pos,
                tokens.iter().map(|b| *b as char).collect::<Vec<_>>(),
                rest.iter().map(|b| *b as char).collect::<Vec<_>>()
            ),
            ParseErrorKind::FileOpenError(err) => format!("failed to open file, details: {}", err),
            ParseErrorKind::MemoryMapError(err) => {
                format!("failed to memory map file, details: {}", err)
            }
        };
        Err(ParseError { msg, kind: self })
    }
}

impl Display for ParseErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct ParseError {
    pub(crate) msg: String,
    pub(crate) kind: ParseErrorKind,
}

impl Into<JsValue> for ParseError {
    fn into(self) -> JsValue {
        JsValue::from(format!(
            "spacey error occurred: {}, {}",
            self.kind, self.msg,
        ))
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum ImpKind {
    Stack,
    Arithmetic,
    Heap,
    Flow,
    IO,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum CommandKind {
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
            | CommandKind::JumpNegative => Some(ParamKind::Label("".to_string().into(), 0)),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum ParamKind {
    Number(i32),
    Label(Rc<str>, usize),
}

/// Intermediate representation for a whitespace instruction. Using the term IR here because I
/// might turn this crate also into a whitespace compiler in the future.
/// Contains data as well as metadata about whitespace instructions
#[wasm_bindgen]
#[derive(Debug, PartialEq, Clone)]
pub struct Instruction {
    pub(crate) imp: ImpKind,
    pub(crate) cmd: CommandKind,
    pub(crate) param: Option<ParamKind>,
    pub(crate) token_index: usize,
    pub(crate) instruction_index: usize,
}

/// The component responsible for reading and parsing the source file
#[wasm_bindgen]
#[derive(Debug)]
pub struct Parser {
    #[cfg(not(target_arch = "wasm32"))]
    source: Mmap,
    #[cfg(target_arch = "wasm32")]
    source: Vec<u8>,
    token_index: usize,
    instruction_index: usize,
}

#[wasm_bindgen]
impl Parser {
    #[cfg(not(target_arch = "wasm32"))]
    pub fn new(file_name: &str) -> Result<Parser, ParseError> {
        let file = match File::open(&file_name) {
            Ok(content) => content,
            Err(err) => return ParseErrorKind::FileOpenError(Box::new(err)).throw(),
        };
        let source = unsafe {
            match Mmap::map(&file) {
                Ok(content) => content,
                Err(err) => return ParseErrorKind::MemoryMapError(Box::new(err)).throw(),
            }
        };
        let index = 0;

        Ok(Parser {
            source,
            token_index: index,
            instruction_index: index,
        })
    }

    #[cfg(target_arch = "wasm32")]
    pub fn new(source: &str) -> Result<Parser, ParseError> {
        let index = 0;

        Ok(Parser {
            source: source.to_string().as_bytes().to_vec(),
            token_index: index,
            instruction_index: index,
        })
    }

    fn next(&mut self) -> Option<u8> {
        let tokens = vec![SPACE, TAB, LINE_FEED];
        while self.token_index < self.source.len() {
            let token = self.source[self.token_index];
            self.token_index += 1;
            if tokens.contains(&token) {
                return Some(token);
            }
        }

        None
    }

    fn imp(&mut self) -> Option<Result<ImpKind, ParseError>> {
        let val = self.next()?;
        match val {
            SPACE => Some(Ok(ImpKind::Stack)),
            TAB => {
                if let Some(val) = self.next() {
                    match val {
                        SPACE => Some(Ok(ImpKind::Arithmetic)),
                        TAB => Some(Ok(ImpKind::Heap)),
                        LINE_FEED => Some(Ok(ImpKind::IO)),
                        _ => Some(
                            ParseErrorKind::UnexpectedToken(
                                self.token_index,
                                val,
                                vec![SPACE, TAB, LINE_FEED],
                            )
                            .throw(),
                        ),
                    }
                } else {
                    Some(
                        ParseErrorKind::UnexpectedToken(
                            self.token_index,
                            val,
                            vec![SPACE, TAB, LINE_FEED],
                        )
                        .throw(),
                    )
                }
            }
            LINE_FEED => Some(Ok(ImpKind::Flow)),
            _ => Some(
                ParseErrorKind::UnexpectedToken(self.token_index, val, vec![SPACE, TAB, LINE_FEED])
                    .throw(),
            ),
        }
    }

    fn stack(&mut self) -> Option<Result<CommandKind, ParseError>> {
        let val = self.next()?;
        match val {
            SPACE => Some(Ok(CommandKind::PushStack)),
            TAB => {
                if let Some(val) = self.next() {
                    return match val {
                        SPACE => Some(Ok(CommandKind::CopyNthStack)),
                        LINE_FEED => Some(Ok(CommandKind::SlideNStack)),
                        _ => Some(
                            ParseErrorKind::UnexpectedToken(
                                self.token_index,
                                val,
                                vec![SPACE, LINE_FEED],
                            )
                            .throw(),
                        ),
                    };
                }
                Some(
                    ParseErrorKind::InvalidToken(
                        self.token_index,
                        vec![SPACE, LINE_FEED],
                        self.source[self.token_index..].to_vec(),
                    )
                    .throw(),
                )
            }
            LINE_FEED => {
                if let Some(val) = self.next() {
                    return match val {
                        SPACE => Some(Ok(CommandKind::DuplicateStack)),
                        TAB => Some(Ok(CommandKind::SwapStack)),
                        LINE_FEED => Some(Ok(CommandKind::DiscardStack)),
                        _ => Some(
                            ParseErrorKind::UnexpectedToken(
                                self.token_index,
                                val,
                                vec![SPACE, TAB, LINE_FEED],
                            )
                            .throw(),
                        ),
                    };
                }
                Some(
                    ParseErrorKind::InvalidToken(
                        self.token_index,
                        vec![SPACE, TAB, LINE_FEED],
                        self.source[self.token_index..].to_vec(),
                    )
                    .throw(),
                )
            }
            _ => Some(
                ParseErrorKind::UnexpectedToken(self.token_index, val, vec![SPACE, TAB, LINE_FEED])
                    .throw(),
            ),
        }
    }

    fn arithmetic(&mut self) -> Option<Result<CommandKind, ParseError>> {
        let val = self.next()?;
        match val {
            SPACE => {
                if let Some(val) = self.next() {
                    return match val {
                        SPACE => Some(Ok(CommandKind::Add)),
                        TAB => Some(Ok(CommandKind::Subtract)),
                        LINE_FEED => Some(Ok(CommandKind::Multiply)),
                        _ => Some(
                            ParseErrorKind::UnexpectedToken(
                                self.token_index,
                                val,
                                vec![SPACE, TAB, LINE_FEED],
                            )
                            .throw(),
                        ),
                    };
                }

                Some(
                    ParseErrorKind::InvalidToken(
                        self.token_index,
                        vec![SPACE, TAB, LINE_FEED],
                        self.source[self.token_index..].to_vec(),
                    )
                    .throw(),
                )
            }
            TAB => {
                if let Some(val) = self.next() {
                    return match val {
                        SPACE => Some(Ok(CommandKind::IntegerDivision)),
                        TAB => Some(Ok(CommandKind::Modulo)),
                        _ => Some(
                            ParseErrorKind::InvalidToken(
                                self.token_index,
                                vec![SPACE, TAB],
                                self.source[self.token_index..].to_vec(),
                            )
                            .throw(),
                        ),
                    };
                }
                Some(
                    ParseErrorKind::InvalidToken(
                        self.token_index,
                        vec![SPACE, TAB],
                        self.source[self.token_index..].to_vec(),
                    )
                    .throw(),
                )
            }
            _ => Some(
                ParseErrorKind::UnexpectedToken(self.token_index, val, vec![SPACE, TAB]).throw(),
            ),
        }
    }

    fn heap(&mut self) -> Option<Result<CommandKind, ParseError>> {
        let val = self.next()?;
        match val {
            SPACE => Some(Ok(CommandKind::StoreHeap)),
            TAB => Some(Ok(CommandKind::RetrieveHeap)),
            _ => Some(
                ParseErrorKind::InvalidToken(
                    self.token_index,
                    vec![SPACE, TAB],
                    self.source[self.token_index..].to_vec(),
                )
                .throw(),
            ),
        }
    }

    fn flow(&mut self) -> Option<Result<CommandKind, ParseError>> {
        let val = self.next()?;
        match val {
            SPACE => {
                if let Some(val) = self.next() {
                    return match val {
                        SPACE => Some(Ok(CommandKind::Mark)),
                        TAB => Some(Ok(CommandKind::Call)),
                        LINE_FEED => Some(Ok(CommandKind::Jump)),
                        _ => Some(
                            ParseErrorKind::UnexpectedToken(
                                self.token_index,
                                val,
                                vec![SPACE, TAB, LINE_FEED],
                            )
                            .throw(),
                        ),
                    };
                }

                Some(
                    ParseErrorKind::InvalidToken(
                        self.token_index,
                        vec![SPACE, TAB, LINE_FEED],
                        self.source[self.token_index..].to_vec(),
                    )
                    .throw(),
                )
            }
            TAB => {
                if let Some(val) = self.next() {
                    return match val {
                        SPACE => Some(Ok(CommandKind::JumpZero)),
                        TAB => Some(Ok(CommandKind::JumpNegative)),
                        LINE_FEED => Some(Ok(CommandKind::Return)),
                        _ => Some(
                            ParseErrorKind::UnexpectedToken(
                                self.token_index,
                                val,
                                vec![SPACE, TAB, LINE_FEED],
                            )
                            .throw(),
                        ),
                    };
                }
                Some(
                    ParseErrorKind::InvalidToken(
                        self.token_index,
                        vec![SPACE, TAB, LINE_FEED],
                        self.source[self.token_index..].to_vec(),
                    )
                    .throw(),
                )
            }
            LINE_FEED => {
                if let Some(val) = self.next() {
                    return match val {
                        LINE_FEED => Some(Ok(CommandKind::Exit)),
                        _ => Some(
                            ParseErrorKind::UnexpectedToken(self.token_index, val, vec![LINE_FEED])
                                .throw(),
                        ),
                    };
                }

                Some(
                    ParseErrorKind::UnexpectedToken(self.token_index, val, vec![LINE_FEED]).throw(),
                )
            }
            _ => Some(
                ParseErrorKind::UnexpectedToken(self.token_index, val, vec![SPACE, TAB, LINE_FEED])
                    .throw(),
            ),
        }
    }

    fn io(&mut self) -> Option<Result<CommandKind, ParseError>> {
        let val = self.next()?;
        match val {
            SPACE => {
                if let Some(val) = self.next() {
                    return match val {
                        SPACE => Some(Ok(CommandKind::OutCharacter)),
                        TAB => Some(Ok(CommandKind::OutInteger)),
                        _ => Some(
                            ParseErrorKind::UnexpectedToken(
                                self.token_index,
                                val,
                                vec![SPACE, TAB],
                            )
                            .throw(),
                        ),
                    };
                }

                Some(
                    ParseErrorKind::UnexpectedToken(self.token_index, val, vec![SPACE, TAB])
                        .throw(),
                )
            }
            TAB => {
                if let Some(val) = self.next() {
                    return match val {
                        SPACE => Some(Ok(CommandKind::ReadCharacter)),
                        TAB => Some(Ok(CommandKind::ReadInteger)),
                        _ => Some(
                            ParseErrorKind::UnexpectedToken(
                                self.token_index,
                                val,
                                vec![SPACE, TAB],
                            )
                            .throw(),
                        ),
                    };
                }

                Some(
                    ParseErrorKind::UnexpectedToken(self.token_index, val, vec![SPACE, TAB])
                        .throw(),
                )
            }
            _ => Some(
                ParseErrorKind::UnexpectedToken(self.token_index, val, vec![SPACE, TAB]).throw(),
            ),
        }
    }

    fn cmd(&mut self, imp: ImpKind) -> Option<Result<CommandKind, ParseError>> {
        match imp {
            ImpKind::Stack => Some(self.stack()?),
            ImpKind::Arithmetic => Some(self.arithmetic()?),
            ImpKind::Heap => Some(self.heap()?),
            ImpKind::Flow => Some(self.flow()?),
            ImpKind::IO => Some(self.io()?),
        }
    }

    fn number(&mut self, sign: i32) -> Option<Result<ParamKind, ParseError>> {
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
                    failure = Some(
                        ParseErrorKind::UnexpectedToken(self.token_index, val, vec![SPACE, TAB])
                            .throw(),
                    );
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
            res += val << place;
            place += 1;
        }

        Some(Ok(ParamKind::Number(sign * res)))
    }

    fn label(&mut self) -> Option<Result<ParamKind, ParseError>> {
        let mut failure = None;
        let mut result = Vec::new();
        while let Some(val) = self.next() {
            result.push(match val {
                SPACE => SPACE,
                TAB => TAB,
                LINE_FEED => break,
                _ => {
                    failure = Some(
                        ParseErrorKind::UnexpectedToken(
                            self.token_index,
                            val,
                            vec![SPACE, TAB, LINE_FEED],
                        )
                        .throw(),
                    );
                    break;
                }
            });
        }
        if failure.is_some() {
            return failure;
        }
        let result = String::from_utf8(result).ok()?;

        Some(Ok(ParamKind::Label(result.into(), 0)))
    }

    fn param(&mut self, kind: ParamKind) -> Option<Result<ParamKind, ParseError>> {
        match kind {
            ParamKind::Label(..) => self.label(),
            ParamKind::Number(_) => {
                if let Some(val) = self.next() {
                    return match val {
                        SPACE => self.number(1),
                        TAB => self.number(-1),
                        _ => Some(
                            ParseErrorKind::UnexpectedToken(
                                self.token_index,
                                val,
                                vec![SPACE, TAB],
                            )
                            .throw(),
                        ),
                    };
                }
                Some(
                    ParseErrorKind::InvalidToken(
                        self.token_index,
                        vec![SPACE, TAB],
                        self.source[self.token_index..].to_vec(),
                    )
                    .throw(),
                )
            }
        }
    }

    fn instruction(&mut self) -> Option<Result<Instruction, ParseError>> {
        let start_index = self.token_index;
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
                let index = self.instruction_index;
                let instr = Instruction {
                    imp,
                    cmd,
                    param,
                    token_index: start_index,
                    instruction_index: index,
                };
                self.instruction_index += 1;

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

impl Iterator for &mut Parser {
    type Item = Result<Instruction, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.instruction()
    }
}

#[cfg(test)]
mod tests {
    use super::{CommandKind, ImpKind, Instruction, ParamKind, ParseError, Parser};

    fn test_parse(parser: &mut Parser, results: Vec<Instruction>) -> Result<(), ParseError> {
        let mut i = 0;
        for instr in parser {
            let instr = instr?;
            if i == results.len() {
                break;
            }
            assert_eq!(instr, results[i]);
            i += 1;
        }

        Ok(())
    }

    #[test]
    fn parse_stack() -> Result<(), ParseError> {
        let mut parser = Parser::new("ws/parse_stack.ws")?;
        let results = vec![
            Instruction {
                imp: ImpKind::Stack,
                cmd: CommandKind::PushStack,
                param: Some(ParamKind::Number(64)),
                token_index: 0,
                instruction_index: 0,
            },
            Instruction {
                imp: ImpKind::Stack,
                cmd: CommandKind::DuplicateStack,
                param: None,
                token_index: 11,
                instruction_index: 1,
            },
            Instruction {
                imp: ImpKind::Stack,
                cmd: CommandKind::CopyNthStack,
                param: Some(ParamKind::Number(64)),
                token_index: 14,
                instruction_index: 2,
            },
            Instruction {
                imp: ImpKind::Stack,
                cmd: CommandKind::SwapStack,
                param: None,
                token_index: 26,
                instruction_index: 3,
            },
            Instruction {
                imp: ImpKind::Stack,
                cmd: CommandKind::DiscardStack,
                param: None,
                token_index: 29,
                instruction_index: 4,
            },
            Instruction {
                imp: ImpKind::Stack,
                cmd: CommandKind::SlideNStack,
                param: Some(ParamKind::Number(64)),
                token_index: 32,
                instruction_index: 5,
            },
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::Exit,
                param: None,
                token_index: 44,
                instruction_index: 6,
            },
        ];

        test_parse(&mut parser, results)
    }

    #[test]
    fn parse_arithmetic() -> Result<(), ParseError> {
        let mut parser = Parser::new("ws/parse_arithmetic.ws")?;
        let results = vec![
            Instruction {
                imp: ImpKind::Arithmetic,
                cmd: CommandKind::Add,
                param: None,
                token_index: 0,
                instruction_index: 0,
            },
            Instruction {
                imp: ImpKind::Arithmetic,
                cmd: CommandKind::Subtract,
                param: None,
                token_index: 4,
                instruction_index: 1,
            },
            Instruction {
                imp: ImpKind::Arithmetic,
                cmd: CommandKind::Multiply,
                param: None,
                token_index: 8,
                instruction_index: 2,
            },
            Instruction {
                imp: ImpKind::Arithmetic,
                cmd: CommandKind::IntegerDivision,
                param: None,
                token_index: 12,
                instruction_index: 3,
            },
            Instruction {
                imp: ImpKind::Arithmetic,
                cmd: CommandKind::Modulo,
                param: None,
                token_index: 16,
                instruction_index: 4,
            },
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::Exit,
                param: None,
                token_index: 20,
                instruction_index: 5,
            },
        ];

        test_parse(&mut parser, results)
    }

    #[test]
    fn parse_heap() -> Result<(), ParseError> {
        let mut parser = Parser::new("ws/parse_heap.ws")?;
        let results = vec![
            Instruction {
                imp: ImpKind::Heap,
                cmd: CommandKind::StoreHeap,
                param: None,
                token_index: 0,
                instruction_index: 0,
            },
            Instruction {
                imp: ImpKind::Heap,
                cmd: CommandKind::RetrieveHeap,
                param: None,
                token_index: 3,
                instruction_index: 1,
            },
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::Exit,
                param: None,
                token_index: 6,
                instruction_index: 2,
            },
        ];

        test_parse(&mut parser, results)
    }

    #[test]
    fn parse_flow() -> Result<(), ParseError> {
        let mut parser = Parser::new("ws/parse_flow.ws")?;
        let results = vec![
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::Mark,
                param: Some(ParamKind::Label(" \t \t \t".to_string().into(), 0)),
                token_index: 0,
                instruction_index: 0,
            },
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::Call,
                param: Some(ParamKind::Label(" \t \t \t".to_string().into(), 0)),
                token_index: 10,
                instruction_index: 1,
            },
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::Jump,
                param: Some(ParamKind::Label(" \t \t \t".to_string().into(), 0)),
                token_index: 20,
                instruction_index: 2,
            },
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::JumpZero,
                param: Some(ParamKind::Label(" \t \t \t".to_string().into(), 0)),
                token_index: 30,
                instruction_index: 3,
            },
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::JumpNegative,
                param: Some(ParamKind::Label(" \t \t \t".to_string().into(), 0)),
                token_index: 40,
                instruction_index: 4,
            },
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::Return,
                param: None,
                token_index: 50,
                instruction_index: 5,
            },
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::Exit,
                param: None,
                token_index: 53,
                instruction_index: 6,
            },
        ];

        test_parse(&mut parser, results)
    }

    #[test]
    fn parse_io() -> Result<(), ParseError> {
        let mut parser = Parser::new("ws/parse_io.ws")?;
        let results = vec![
            Instruction {
                imp: ImpKind::IO,
                cmd: CommandKind::OutCharacter,
                param: None,
                token_index: 0,
                instruction_index: 0,
            },
            Instruction {
                imp: ImpKind::IO,
                cmd: CommandKind::OutInteger,
                param: None,
                token_index: 4,
                instruction_index: 1,
            },
            Instruction {
                imp: ImpKind::IO,
                cmd: CommandKind::ReadCharacter,
                param: None,
                token_index: 8,
                instruction_index: 2,
            },
            Instruction {
                imp: ImpKind::IO,
                cmd: CommandKind::ReadInteger,
                param: None,
                token_index: 12,
                instruction_index: 3,
            },
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::Exit,
                param: None,
                token_index: 16,
                instruction_index: 4,
            },
        ];

        test_parse(&mut parser, results)
    }
}
