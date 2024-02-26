use crate::parser::ParseErrorKind;
#[cfg(not(target_arch = "wasm32"))]
use memmap::Mmap;
#[cfg(not(target_arch = "wasm32"))]
use std::fs::File;

use crate::ParseError;
use crate::Parser;

const R_ANGLE: u8 = b'>';
const L_ANGLE: u8 = b'<';
const PLUS: u8 = b'+';
const MINUS: u8 = b'-';
const DOT: u8 = b'.';
const COMMA: u8 = b',';
const L_BRACKET: u8 = b'[';
const R_BRACKET: u8 = b']';

#[derive(Debug)]
pub struct BfParser {
    #[cfg(not(target_arch = "wasm32"))]
    source: Mmap,
    #[cfg(target_arch = "wasm32")]
    source: Vec<u8>,
    token_index: usize,
}

impl Parser for BfParser {
    fn instruction(&mut self) -> Option<Result<Box<dyn crate::Instr>, crate::ParseError>> {
        None
    }
}

impl BfParser {
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

        Ok(Box::new(BfParser {
            source,
            token_index: index,
        }))
    }

    #[cfg(target_arch = "wasm32")]
    pub fn new(source: &str) -> Result<Box<dyn Parser>, ParseError> {
        let index = 0;

        Ok(Box::new(BfParser {
            source: source.to_string().as_bytes().to_vec(),
            token_index: index,
        }))
    }
}
