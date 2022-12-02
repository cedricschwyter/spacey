use memmap::Mmap;

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
