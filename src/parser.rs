use std::{
    error::Error,
    fmt::{Debug, Display},
    str::FromStr,
};

use wasm_bindgen::JsValue;

use crate::Instruction;

pub enum SourceType {
    Whitespace,
    Malbolge,
    Brainfuck,
}

impl FromStr for SourceType {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "whitespace" => Ok(SourceType::Whitespace),
            "malbolge" => Ok(SourceType::Malbolge),
            "brainfuck" => Ok(SourceType::Brainfuck),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub(crate) enum ParseErrorKind {
    InvalidToken(usize, Vec<u8>, Vec<u8>),
    UnexpectedToken(usize, u8, Vec<u8>),
    #[allow(unused)]
    FileOpenError(Box<dyn Error>),
    #[allow(unused)]
    MemoryMapError(Box<dyn Error>),
}

impl ParseErrorKind {
    pub(crate) fn throw<T>(self) -> Result<T, ParseError> {
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
            ParseErrorKind::FileOpenError(err) => {
                format!("failed to open file, details: {}", err)
            }
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

pub trait Instr: Debug {
    fn translate(&self) -> Result<Instruction, ParseError>;
}

pub trait Parser {
    fn instruction(&mut self) -> Option<Result<Box<dyn Instr>, ParseError>>;
}

impl Iterator for &mut Box<dyn Parser> {
    type Item = Result<Box<dyn Instr>, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.instruction()
    }
}
