use crate::parser::Instr;
use crate::parser::ParseError;
use crate::parser::ParseErrorKind;
use crate::parser::Parser;
use crate::Instruction;
#[cfg(not(target_arch = "wasm32"))]
use memmap::Mmap;
#[cfg(not(target_arch = "wasm32"))]
use std::fs::File;
use std::rc::Rc;
use wasm_bindgen::prelude::wasm_bindgen;

pub const SPACE: u8 = b' ';
pub const TAB: u8 = b'\t';
pub const LINE_FEED: u8 = b'\n';

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum WsImpKind {
    Stack,
    Arithmetic,
    Heap,
    Flow,
    IO,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum WsCommandKind {
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

impl WsCommandKind {
    fn param_kind(&self) -> Option<WsParamKind> {
        match self {
            WsCommandKind::PushStack | WsCommandKind::CopyNthStack | WsCommandKind::SlideNStack => {
                Some(WsParamKind::Number(0))
            }
            WsCommandKind::Mark
            | WsCommandKind::Call
            | WsCommandKind::Jump
            | WsCommandKind::JumpZero
            | WsCommandKind::JumpNegative => Some(WsParamKind::Label("".to_string().into(), 0)),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum WsParamKind {
    Number(i32),
    Label(Rc<str>, usize),
}

/// Intermediate representation for a whitespace instruction. Using the term IR here because I
/// might turn this crate also into a whitespace compiler in the future.
/// Contains data as well as metadata about whitespace instructions
#[wasm_bindgen]
#[derive(Debug, PartialEq, Clone)]
pub struct WsInstruction {
    pub(crate) imp: WsImpKind,
    pub(crate) cmd: WsCommandKind,
    pub(crate) param: Option<WsParamKind>,
    pub(crate) token_index: usize,
    pub(crate) instruction_index: usize,
}

impl Instr for WsInstruction {
    fn translate(&self) -> Result<Instruction, ParseError> {
        todo!();
        Ok(Instruction::Add)
    }
}

/// The component responsible for reading and parsing the source file
#[wasm_bindgen]
#[derive(Debug)]
pub struct WsParser {
    #[cfg(not(target_arch = "wasm32"))]
    source: Mmap,
    #[cfg(target_arch = "wasm32")]
    source: Vec<u8>,
    token_index: usize,
    instruction_index: usize,
}

impl Parser for WsParser {
    fn instruction(&mut self) -> Option<Result<Box<dyn Instr>, ParseError>> {
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
                let instr = WsInstruction {
                    imp,
                    cmd,
                    param,
                    token_index: start_index,
                    instruction_index: index,
                };
                self.instruction_index += 1;

                return Some(Ok(Box::new(instr)));
            } else if let Err(err) = cmd {
                return Some(Err(err));
            }
        } else if let Err(err) = imp {
            return Some(Err(err));
        }

        None
    }
}

impl WsParser {
    #[cfg(not(target_arch = "wasm32"))]
    pub fn new(file_name: &str) -> Result<Box<dyn Parser>, ParseError> {
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

        Ok(Box::new(WsParser {
            source,
            token_index: index,
            instruction_index: index,
        }))
    }

    #[cfg(target_arch = "wasm32")]
    pub fn new(source: &str) -> Result<WsParser, ParseError> {
        let index = 0;

        Ok(WsParser {
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

    fn imp(&mut self) -> Option<Result<WsImpKind, ParseError>> {
        let val = self.next()?;
        match val {
            SPACE => Some(Ok(WsImpKind::Stack)),
            TAB => {
                if let Some(val) = self.next() {
                    match val {
                        SPACE => Some(Ok(WsImpKind::Arithmetic)),
                        TAB => Some(Ok(WsImpKind::Heap)),
                        LINE_FEED => Some(Ok(WsImpKind::IO)),
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
            LINE_FEED => Some(Ok(WsImpKind::Flow)),
            _ => Some(
                ParseErrorKind::UnexpectedToken(self.token_index, val, vec![SPACE, TAB, LINE_FEED])
                    .throw(),
            ),
        }
    }

    fn stack(&mut self) -> Option<Result<WsCommandKind, ParseError>> {
        let val = self.next()?;
        match val {
            SPACE => Some(Ok(WsCommandKind::PushStack)),
            TAB => {
                if let Some(val) = self.next() {
                    return match val {
                        SPACE => Some(Ok(WsCommandKind::CopyNthStack)),
                        LINE_FEED => Some(Ok(WsCommandKind::SlideNStack)),
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
                        SPACE => Some(Ok(WsCommandKind::DuplicateStack)),
                        TAB => Some(Ok(WsCommandKind::SwapStack)),
                        LINE_FEED => Some(Ok(WsCommandKind::DiscardStack)),
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

    fn arithmetic(&mut self) -> Option<Result<WsCommandKind, ParseError>> {
        let val = self.next()?;
        match val {
            SPACE => {
                if let Some(val) = self.next() {
                    return match val {
                        SPACE => Some(Ok(WsCommandKind::Add)),
                        TAB => Some(Ok(WsCommandKind::Subtract)),
                        LINE_FEED => Some(Ok(WsCommandKind::Multiply)),
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
                        SPACE => Some(Ok(WsCommandKind::IntegerDivision)),
                        TAB => Some(Ok(WsCommandKind::Modulo)),
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

    fn heap(&mut self) -> Option<Result<WsCommandKind, ParseError>> {
        let val = self.next()?;
        match val {
            SPACE => Some(Ok(WsCommandKind::StoreHeap)),
            TAB => Some(Ok(WsCommandKind::RetrieveHeap)),
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

    fn flow(&mut self) -> Option<Result<WsCommandKind, ParseError>> {
        let val = self.next()?;
        match val {
            SPACE => {
                if let Some(val) = self.next() {
                    return match val {
                        SPACE => Some(Ok(WsCommandKind::Mark)),
                        TAB => Some(Ok(WsCommandKind::Call)),
                        LINE_FEED => Some(Ok(WsCommandKind::Jump)),
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
                        SPACE => Some(Ok(WsCommandKind::JumpZero)),
                        TAB => Some(Ok(WsCommandKind::JumpNegative)),
                        LINE_FEED => Some(Ok(WsCommandKind::Return)),
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
                        LINE_FEED => Some(Ok(WsCommandKind::Exit)),
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

    fn io(&mut self) -> Option<Result<WsCommandKind, ParseError>> {
        let val = self.next()?;
        match val {
            SPACE => {
                if let Some(val) = self.next() {
                    return match val {
                        SPACE => Some(Ok(WsCommandKind::OutCharacter)),
                        TAB => Some(Ok(WsCommandKind::OutInteger)),
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
                        SPACE => Some(Ok(WsCommandKind::ReadCharacter)),
                        TAB => Some(Ok(WsCommandKind::ReadInteger)),
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

    fn cmd(&mut self, imp: WsImpKind) -> Option<Result<WsCommandKind, ParseError>> {
        match imp {
            WsImpKind::Stack => Some(self.stack()?),
            WsImpKind::Arithmetic => Some(self.arithmetic()?),
            WsImpKind::Heap => Some(self.heap()?),
            WsImpKind::Flow => Some(self.flow()?),
            WsImpKind::IO => Some(self.io()?),
        }
    }

    fn number(&mut self, sign: i32) -> Option<Result<WsParamKind, ParseError>> {
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

        Some(Ok(WsParamKind::Number(sign * res)))
    }

    fn label(&mut self) -> Option<Result<WsParamKind, ParseError>> {
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

        Some(Ok(WsParamKind::Label(result.into(), 0)))
    }

    fn param(&mut self, kind: WsParamKind) -> Option<Result<WsParamKind, ParseError>> {
        match kind {
            WsParamKind::Label(..) => self.label(),
            WsParamKind::Number(_) => {
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
}

#[cfg(test)]
mod tests {
    use crate::parser::{Instr, Parser};

    use super::{ParseError, WsCommandKind, WsImpKind, WsInstruction, WsParamKind, WsParser};

    fn test_parse(
        parser: &mut Box<dyn Parser>,
        results: Vec<Box<dyn Instr>>,
    ) -> Result<(), ParseError> {
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
        let mut parser = WsParser::new("ws/parse_stack.ws")?;
        let results = vec![
            Box::new(WsInstruction {
                imp: WsImpKind::Stack,
                cmd: WsCommandKind::PushStack,
                param: Some(WsParamKind::Number(64)),
                token_index: 0,
                instruction_index: 0,
            }),
            Box::new(WsInstruction {
                imp: WsImpKind::Stack,
                cmd: WsCommandKind::DuplicateStack,
                param: None,
                token_index: 11,
                instruction_index: 1,
            }),
            Box::new(WsInstruction {
                imp: WsImpKind::Stack,
                cmd: WsCommandKind::CopyNthStack,
                param: Some(WsParamKind::Number(64)),
                token_index: 14,
                instruction_index: 2,
            }),
            Box::new(WsInstruction {
                imp: WsImpKind::Stack,
                cmd: WsCommandKind::SwapStack,
                param: None,
                token_index: 26,
                instruction_index: 3,
            }),
            Box::new(WsInstruction {
                imp: WsImpKind::Stack,
                cmd: WsCommandKind::DiscardStack,
                param: None,
                token_index: 29,
                instruction_index: 4,
            }),
            Box::new(WsInstruction {
                imp: WsImpKind::Stack,
                cmd: WsCommandKind::SlideNStack,
                param: Some(WsParamKind::Number(64)),
                token_index: 32,
                instruction_index: 5,
            }),
            Box::new(WsInstruction {
                imp: WsImpKind::Flow,
                cmd: WsCommandKind::Exit,
                param: None,
                token_index: 44,
                instruction_index: 6,
            }),
        ];

        test_parse(&mut parser, results)
    }

    #[test]
    fn parse_arithmetic() -> Result<(), ParseError> {
        let mut parser = WsParser::new("ws/parse_arithmetic.ws")?;
        let results = vec![
            WsInstruction {
                imp: WsImpKind::Arithmetic,
                cmd: WsCommandKind::Add,
                param: None,
                token_index: 0,
                instruction_index: 0,
            },
            WsInstruction {
                imp: WsImpKind::Arithmetic,
                cmd: WsCommandKind::Subtract,
                param: None,
                token_index: 4,
                instruction_index: 1,
            },
            WsInstruction {
                imp: WsImpKind::Arithmetic,
                cmd: WsCommandKind::Multiply,
                param: None,
                token_index: 8,
                instruction_index: 2,
            },
            WsInstruction {
                imp: WsImpKind::Arithmetic,
                cmd: WsCommandKind::IntegerDivision,
                param: None,
                token_index: 12,
                instruction_index: 3,
            },
            WsInstruction {
                imp: WsImpKind::Arithmetic,
                cmd: WsCommandKind::Modulo,
                param: None,
                token_index: 16,
                instruction_index: 4,
            },
            WsInstruction {
                imp: WsImpKind::Flow,
                cmd: WsCommandKind::Exit,
                param: None,
                token_index: 20,
                instruction_index: 5,
            },
        ];

        test_parse(&mut parser, results)
    }

    #[test]
    fn parse_heap() -> Result<(), ParseError> {
        let mut parser = WsParser::new("ws/parse_heap.ws")?;
        let results = vec![
            WsInstruction {
                imp: WsImpKind::Heap,
                cmd: WsCommandKind::StoreHeap,
                param: None,
                token_index: 0,
                instruction_index: 0,
            },
            WsInstruction {
                imp: WsImpKind::Heap,
                cmd: WsCommandKind::RetrieveHeap,
                param: None,
                token_index: 3,
                instruction_index: 1,
            },
            WsInstruction {
                imp: WsImpKind::Flow,
                cmd: WsCommandKind::Exit,
                param: None,
                token_index: 6,
                instruction_index: 2,
            },
        ];

        test_parse(&mut parser, results)
    }

    #[test]
    fn parse_flow() -> Result<(), ParseError> {
        let mut parser = WsParser::new("ws/parse_flow.ws")?;
        let results = vec![
            WsInstruction {
                imp: WsImpKind::Flow,
                cmd: WsCommandKind::Mark,
                param: Some(WsParamKind::Label(" \t \t \t".to_string().into(), 0)),
                token_index: 0,
                instruction_index: 0,
            },
            WsInstruction {
                imp: WsImpKind::Flow,
                cmd: WsCommandKind::Call,
                param: Some(WsParamKind::Label(" \t \t \t".to_string().into(), 0)),
                token_index: 10,
                instruction_index: 1,
            },
            WsInstruction {
                imp: WsImpKind::Flow,
                cmd: WsCommandKind::Jump,
                param: Some(WsParamKind::Label(" \t \t \t".to_string().into(), 0)),
                token_index: 20,
                instruction_index: 2,
            },
            WsInstruction {
                imp: WsImpKind::Flow,
                cmd: WsCommandKind::JumpZero,
                param: Some(WsParamKind::Label(" \t \t \t".to_string().into(), 0)),
                token_index: 30,
                instruction_index: 3,
            },
            WsInstruction {
                imp: WsImpKind::Flow,
                cmd: WsCommandKind::JumpNegative,
                param: Some(WsParamKind::Label(" \t \t \t".to_string().into(), 0)),
                token_index: 40,
                instruction_index: 4,
            },
            WsInstruction {
                imp: WsImpKind::Flow,
                cmd: WsCommandKind::Return,
                param: None,
                token_index: 50,
                instruction_index: 5,
            },
            WsInstruction {
                imp: WsImpKind::Flow,
                cmd: WsCommandKind::Exit,
                param: None,
                token_index: 53,
                instruction_index: 6,
            },
        ];

        test_parse(&mut parser, results)
    }

    #[test]
    fn parse_io() -> Result<(), ParseError> {
        let mut parser = WsParser::new("ws/parse_io.ws")?;
        let results = vec![
            WsInstruction {
                imp: WsImpKind::IO,
                cmd: WsCommandKind::OutCharacter,
                param: None,
                token_index: 0,
                instruction_index: 0,
            },
            WsInstruction {
                imp: WsImpKind::IO,
                cmd: WsCommandKind::OutInteger,
                param: None,
                token_index: 4,
                instruction_index: 1,
            },
            WsInstruction {
                imp: WsImpKind::IO,
                cmd: WsCommandKind::ReadCharacter,
                param: None,
                token_index: 8,
                instruction_index: 2,
            },
            WsInstruction {
                imp: WsImpKind::IO,
                cmd: WsCommandKind::ReadInteger,
                param: None,
                token_index: 12,
                instruction_index: 3,
            },
            WsInstruction {
                imp: WsImpKind::Flow,
                cmd: WsCommandKind::Exit,
                param: None,
                token_index: 16,
                instruction_index: 4,
            },
        ];

        test_parse(&mut parser, results)
    }
}
