#![feature(test)]

pub mod interpret;
pub mod parse;

pub use interpret::Interpreter;
pub use parse::{Instruction, Parser};
