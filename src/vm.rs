use crate::parser::{ParseError, SourceType};
use crate::{Instruction, WsParser};
#[cfg(not(target_arch = "wasm32"))]
use getch::Getch;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::fmt::Display;
use std::io::{stdin, stdout, Write};
use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::JsValue;

#[allow(unused)]
const DEFAULT_HEAP_SIZE: usize = 524288;

/// The root component for the virtual machine
#[wasm_bindgen]
pub struct Vm {
    config: VmConfig,
    stack: Vec<i32>,
    call_stack: Vec<usize>,
    heap: Vec<i32>,
    instruction_pointer: usize,
    instructions: Vec<Instruction>,
    done: bool,
}

/// Configuration options for the interpreter
#[wasm_bindgen]
pub struct VmConfig {
    source_type: SourceType,
    #[cfg(not(target_arch = "wasm32"))]
    file_name: String,
    #[cfg(target_arch = "wasm32")]
    source: String,
    heap_size: usize,
    raw: bool,
    debug: bool,
    debug_heap: bool,
    suppress_output: bool,
}

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
impl VmConfig {
    /// Creates a new interpreter config with the given arguments
    ///
    /// - `source` the source as a String
    /// - `source_type` the type of the source
    /// - `heap_size` the size of the heap address space (each address holds an i32)
    /// - `raw` print the raw instructions of the parsed source file to stdout
    /// - `debug` print debugging information to stdout when executing an instruction
    /// - `debug_heap` print heap dump to stdout when executing an instruction
    #[wasm_bindgen(constructor)]
    pub fn new(
        source: &str,
        source_type: SourceType,
        heap_size: usize,
        raw: bool,
        debug: bool,
        debug_heap: bool,
        suppress_output: bool,
    ) -> VmConfig {
        VmConfig {
            source_type,
            source: source.to_string(),
            heap_size,
            raw,
            debug,
            debug_heap,
            suppress_output,
        }
    }

    /// Returns a default interpreter configuration with the default heap size
    ///
    /// - `source` the source as a String
    /// - `source_type` the type of the source
    pub fn default_heap(source: &str, source_type: SourceType) -> VmConfig {
        VmConfig {
            source_type,
            source: source.to_string(),
            heap_size: DEFAULT_HEAP_SIZE,
            raw: false,
            debug: false,
            debug_heap: false,
            suppress_output: false,
        }
    }

    /// Returns a default interpreter configuration with no heap
    ///
    /// - `source` the source as a String
    /// - `source_type` the type of the source
    pub fn default_no_heap(source: &str, source_type: SourceType) -> VmConfig {
        VmConfig {
            source_type,
            source: source.to_string(),
            heap_size: 0,
            raw: false,
            debug: false,
            debug_heap: false,
            suppress_output: false,
        }
    }

    /// Returns a default interpreter configuration with the default heap size, suppressing output
    ///
    /// - `source` the source as a String
    /// - `source_type` the type of the source
    pub fn default_heap_suppressed(source: &str, source_type: SourceType) -> VmConfig {
        VmConfig {
            source_type,
            source: source.to_string(),
            heap_size: DEFAULT_HEAP_SIZE,
            raw: false,
            debug: false,
            debug_heap: false,
            suppress_output: true,
        }
    }

    /// Returns a default interpreter configuration with no heap, suppressing output
    ///
    /// - `source` the source as a String
    /// - `source_type` the type of the source
    pub fn default_no_heap_suppressed(source: &str, source_type: SourceType) -> VmConfig {
        VmConfig {
            source_type,
            source: source.to_string(),
            heap_size: 0,
            raw: false,
            debug: false,
            debug_heap: false,
            suppress_output: true,
        }
    }

    /// Returns a default debug interpreter configuration with the default heap size
    ///
    /// - `source` the source as a String
    /// - `source_type` the type of the source
    pub fn debug_heap(source: &str, source_type: SourceType) -> VmConfig {
        VmConfig {
            source_type,
            source: source.to_string(),
            heap_size: DEFAULT_HEAP_SIZE,
            raw: false,
            debug: true,
            debug_heap: true,
            suppress_output: false,
        }
    }

    /// Returns a default debug interpreter configuration with no heap
    ///
    /// - `source` the source as a String
    /// - `source_type` the type of the source
    pub fn debug_no_heap(source: &str, source_type: SourceType) -> VmConfig {
        VmConfig {
            source_type,
            source: source.to_string(),
            heap_size: 0,
            raw: false,
            debug: true,
            debug_heap: false,
            suppress_output: false,
        }
    }

    /// Returns a default debug interpreter configuration to only compute the intermediate
    /// representation of the source
    ///
    /// - `source` the source as a String
    /// - `source_type` the type of the source
    pub fn raw(source: &str, source_type: SourceType) -> VmConfig {
        VmConfig {
            source_type,
            source: source.to_string(),
            heap_size: 0,
            raw: true,
            debug: false,
            debug_heap: false,
            suppress_output: false,
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
impl VmConfig {
    /// Creates a new interpreter config with the given arguments
    ///
    /// - `file_name` the path to the source file on disk
    /// - `source_type` the type of the source
    /// - `heap_size` the size of the heap address space (each address holds an i32)
    /// - `raw` print the IR of the parsed source file to stdout
    /// - `debug` print debugging information to stdout when executing an instruction
    /// - `debug_heap` print heap dump to stdout when executing an instruction
    pub fn new(
        file_name: &str,
        source_type: SourceType,
        heap_size: usize,
        raw: bool,
        debug: bool,
        debug_heap: bool,
        suppress_output: bool,
    ) -> VmConfig {
        VmConfig {
            source_type,
            file_name: file_name.to_string(),
            heap_size,
            raw,
            debug,
            debug_heap,
            suppress_output,
        }
    }

    /// Returns a default interpreter configuration with the default heap size
    ///
    /// - `file_name` the name of the source file on disk
    /// - `source_type` the type of the source
    pub fn default_heap(file_name: &str, source_type: SourceType) -> VmConfig {
        VmConfig {
            source_type,
            file_name: file_name.to_string(),
            heap_size: DEFAULT_HEAP_SIZE,
            raw: false,
            debug: false,
            debug_heap: false,
            suppress_output: false,
        }
    }

    /// Returns a default interpreter configuration with no heap
    ///
    /// - `file_name` the name of the source file on disk
    /// - `source_type` the type of the source
    pub fn default_no_heap(file_name: &str, source_type: SourceType) -> VmConfig {
        VmConfig {
            source_type,
            file_name: file_name.to_string(),
            heap_size: 0,
            raw: false,
            debug: false,
            debug_heap: false,
            suppress_output: false,
        }
    }

    /// Returns a default interpreter configuration with the default heap size, suppressing output
    ///
    /// - `file_name` the name of the source file on disk
    /// - `source_type` the type of the source
    pub fn default_heap_suppressed(file_name: &str, source_type: SourceType) -> VmConfig {
        VmConfig {
            source_type,
            file_name: file_name.to_string(),
            heap_size: DEFAULT_HEAP_SIZE,
            raw: false,
            debug: false,
            debug_heap: false,
            suppress_output: true,
        }
    }

    /// Returns a default interpreter configuration with no heap, suppressing output
    ///
    /// - `file_name` the name of the source file on disk
    /// - `source_type` the type of the source
    pub fn default_no_heap_suppressed(file_name: &str, source_type: SourceType) -> VmConfig {
        VmConfig {
            source_type,
            file_name: file_name.to_string(),
            heap_size: 0,
            raw: false,
            debug: false,
            debug_heap: false,
            suppress_output: true,
        }
    }

    /// Returns a default debug interpreter configuration with the default heap size
    ///
    /// - `file_name` the name of the source file on disk
    /// - `source_type` the type of the source
    pub fn debug_heap(file_name: &str, source_type: SourceType) -> VmConfig {
        VmConfig {
            source_type,
            file_name: file_name.to_string(),
            heap_size: DEFAULT_HEAP_SIZE,
            raw: false,
            debug: true,
            debug_heap: true,
            suppress_output: false,
        }
    }

    /// Returns a default debug interpreter configuration with no heap
    ///
    /// - `file_name` the name of the source file on disk
    /// - `source_type` the type of the source
    pub fn debug_no_heap(file_name: &str, source_type: SourceType) -> VmConfig {
        VmConfig {
            source_type,
            file_name: file_name.to_string(),
            heap_size: 0,
            raw: false,
            debug: true,
            debug_heap: false,
            suppress_output: false,
        }
    }

    /// Returns a default debug interpreter configuration to only compute the intermediate
    /// representation of the source
    ///
    /// - `file_name` the name of the source file on disk
    /// - `source_type` the type of the source
    pub fn raw(file_name: &str, source_type: SourceType) -> VmConfig {
        VmConfig {
            source_type,
            file_name: file_name.to_string(),
            heap_size: 0,
            raw: true,
            debug: false,
            debug_heap: false,
            suppress_output: false,
        }
    }
}

#[derive(Debug)]
enum VmErrorKind {
    TranslateError(ParseError),
    ParseError(ParseError),
    StackUnderflow(Instruction),
    NumberOutOfBoundsError(Instruction, i32, i32, i32),
    NoTermination(Instruction),
    IOError(Instruction),
}

impl Display for VmErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl VmErrorKind {
    fn throw<T>(self) -> Result<T, VmError> {
        let msg = match &self {
            VmErrorKind::TranslateError(err) => format!("error during instruction translation: {}", err),
            VmErrorKind::StackUnderflow(instr) => format!("stack is empty - failed executing: {:?}", instr),
            VmErrorKind::NumberOutOfBoundsError(instr, num, low, high) => format!("number is out of bounds for: {:?}, expected in the closed interval bounded by {} and {}, but was {}", instr, low, high, num),
            VmErrorKind::NoTermination(instr) => format!("no termination instruction after last executed instruction: {:?}", instr),
            VmErrorKind::IOError(instr) => format!("stdin error when executing: {:?}", instr),
            VmErrorKind::ParseError(err) => format!("parse error occurred: {}, {}", err.kind, err.msg)
        };
        Err(VmError { msg, kind: self })
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct VmError {
    msg: String,
    kind: VmErrorKind,
}

impl Into<JsValue> for VmError {
    fn into(self) -> JsValue {
        JsValue::from(format!("spacey error occured: {}, {}", self.kind, self.msg))
    }
}

impl Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[wasm_bindgen]
impl Vm {
    /// Creates a new interpreter with the given arguments
    ///
    /// - `config` The configuration of the interpreter
    pub fn new(config: VmConfig) -> Result<Vm, VmError> {
        #[cfg(not(target_arch = "wasm32"))]
        let source_or_source_file = &config.file_name;
        #[cfg(target_arch = "wasm32")]
        let source_or_source_file = &config.source;
        let mut parser = match config.source_type {
            SourceType::Whitespace => match WsParser::new(source_or_source_file) {
                Ok(content) => content,
                Err(err) => return VmErrorKind::ParseError(err).throw(),
            },
            SourceType::Malbolge => unimplemented!(),
            SourceType::Brainfuck => unimplemented!(),
        };
        let mut instructions = vec![];
        for instr in &mut parser {
            let instr = match instr {
                Ok(content) => content,
                Err(err) => return VmErrorKind::ParseError(err).throw(),
            };
            if config.raw {
                dbg!(&instr);
            }
            let instr = match instr.translate() {
                Ok(instr) => instr,
                Err(err) => return VmErrorKind::TranslateError(err).throw(),
            };
            instructions.push(instr);
        }
        let stack = vec![];
        let call_stack = vec![];
        let heap = vec![0; config.heap_size];
        let mut labels = HashMap::new();
        let instruction_pointer = 0;
        let done = false;

        for (i, instr) in instructions.iter().enumerate() {
            match instr {
                Instruction::Mark(label) => {
                    labels.insert(label.value.clone(), i);
                }
                _ => (),
            }
        }

        for instr in &mut instructions {
            match instr {
                Instruction::Mark(label)
                | Instruction::Call(label)
                | Instruction::Jump(label)
                | Instruction::JumpZero(label)
                | Instruction::JumpNegative(label) => {
                    if let Some(index) = labels.get(&label.value) {
                        label.index = *index;
                    }
                }
                _ => {}
            }
        }

        Ok(Vm {
            config,
            instructions,
            stack,
            call_stack,
            heap,
            instruction_pointer,
            done,
        })
    }

    /// Returns the next instruction to be executed in a `Some` variant. None if the program has
    /// reached its end.
    pub fn next_instruction(&self) -> Option<usize> {
        if self.done {
            return None;
        }
        if self.instruction_pointer < self.instructions.len() {
            Some(self.instruction_pointer)
        } else {
            None
        }
    }

    /// Executes all instructions - runs the program.
    pub fn run(&mut self) -> Result<(), VmError> {
        while let Some(_) = self.next_instruction() {
            self.exec()?;
        }

        let last = &self.instructions[self.instruction_pointer - 1];
        if *last != Instruction::Exit {
            return VmErrorKind::NoTermination(last.clone()).throw();
        }

        Ok(())
    }

    /// Resets the internal interpreter state/the VM without re-parsing the source file
    pub fn reset(&mut self) {
        self.stack.clear();
        self.call_stack.clear();
        self.heap = vec![0; self.heap.len()];
        self.instruction_pointer = 0;
        self.done = false;
    }

    fn generate_debug_heap_dump(&self) -> BTreeMap<usize, i32> {
        let mut heap_map = BTreeMap::new();
        for (addr, val) in self.heap.iter().enumerate() {
            if *val != 0 {
                heap_map.insert(addr, *val);
            }
        }
        heap_map
    }

    fn push_stack(&mut self) -> Result<(), VmError> {
        if let Instruction::PushStack(num) = self.instructions[self.instruction_pointer] {
            self.stack.push(num.value);

            return Ok(());
        };

        unreachable!();
    }

    fn duplicate_stack(&mut self) -> Result<(), VmError> {
        if let Some(val) = self.stack.pop() {
            self.stack.push(val);
            self.stack.push(val);

            return Ok(());
        }

        VmErrorKind::StackUnderflow(self.instructions[self.instruction_pointer].clone()).throw()
    }

    fn copy_nth_stack(&mut self) -> Result<(), VmError> {
        if let Instruction::CopyNthStack(num) = self.instructions[self.instruction_pointer] {
            let addr = num.value;
            if addr < 0 || addr as usize >= self.stack.len() {
                return VmErrorKind::NumberOutOfBoundsError(
                    self.instructions[self.instruction_pointer].clone(),
                    addr,
                    0,
                    self.stack.len() as i32 - 1,
                )
                .throw();
            }
            let addr = addr as usize;
            let val = self.stack[addr];
            self.stack.push(val);

            return Ok(());
        }

        unreachable!();
    }

    fn swap_stack(&mut self) -> Result<(), VmError> {
        if let Some(val) = self.stack.pop() {
            if let Some(other) = self.stack.pop() {
                self.stack.push(val);
                self.stack.push(other);

                return Ok(());
            }

            return VmErrorKind::StackUnderflow(
                self.instructions[self.instruction_pointer].clone(),
            )
            .throw();
        }

        VmErrorKind::StackUnderflow(self.instructions[self.instruction_pointer].clone()).throw()
    }

    fn discard_stack(&mut self) -> Result<(), VmError> {
        if self.stack.pop().is_some() {
            return Ok(());
        }

        VmErrorKind::StackUnderflow(self.instructions[self.instruction_pointer].clone()).throw()
    }

    fn slide_n_stack(&mut self) -> Result<(), VmError> {
        if let Instruction::SlideNStack(num) = self.instructions[self.instruction_pointer] {
            if let Some(top) = self.stack.pop() {
                if num.value < 0 {
                    return VmErrorKind::NumberOutOfBoundsError(
                        self.instructions[self.instruction_pointer].clone(),
                        num.value,
                        0,
                        i32::MAX,
                    )
                    .throw();
                }
                for _i in 0..num.value {
                    self.stack.pop();
                }
                self.stack.push(top);

                return Ok(());
            }

            return VmErrorKind::StackUnderflow(
                self.instructions[self.instruction_pointer].clone(),
            )
            .throw();
        };

        unreachable!();
    }

    fn add(&mut self) -> Result<(), VmError> {
        if let Some(right) = self.stack.pop() {
            if let Some(left) = self.stack.pop() {
                self.stack.push(left + right);

                return Ok(());
            }
        }

        VmErrorKind::StackUnderflow(self.instructions[self.instruction_pointer].clone()).throw()
    }

    fn subtract(&mut self) -> Result<(), VmError> {
        if let Some(right) = self.stack.pop() {
            if let Some(left) = self.stack.pop() {
                self.stack.push(left - right);

                return Ok(());
            }
        }

        VmErrorKind::StackUnderflow(self.instructions[self.instruction_pointer].clone()).throw()
    }

    fn multiply(&mut self) -> Result<(), VmError> {
        if let Some(right) = self.stack.pop() {
            if let Some(left) = self.stack.pop() {
                self.stack.push(left * right);

                return Ok(());
            }
        }

        VmErrorKind::StackUnderflow(self.instructions[self.instruction_pointer].clone()).throw()
    }

    fn integer_division(&mut self) -> Result<(), VmError> {
        if let Some(right) = self.stack.pop() {
            if let Some(left) = self.stack.pop() {
                self.stack.push(left / right);

                return Ok(());
            }
        }

        VmErrorKind::StackUnderflow(self.instructions[self.instruction_pointer].clone()).throw()
    }

    fn modulo(&mut self) -> Result<(), VmError> {
        if let Some(right) = self.stack.pop() {
            if let Some(left) = self.stack.pop() {
                self.stack.push(left % right);

                return Ok(());
            }
        }

        VmErrorKind::StackUnderflow(self.instructions[self.instruction_pointer].clone()).throw()
    }

    fn store_heap(&mut self) -> Result<(), VmError> {
        if let Some(val) = self.stack.pop() {
            if let Some(addr) = self.stack.pop() {
                if addr < 0 || addr as usize >= self.heap.len() {
                    return VmErrorKind::NumberOutOfBoundsError(
                        self.instructions[self.instruction_pointer].clone(),
                        addr,
                        0,
                        self.heap.len() as i32 - 1,
                    )
                    .throw();
                }

                self.heap[addr as usize] = val;

                return Ok(());
            }
        }

        VmErrorKind::StackUnderflow(self.instructions[self.instruction_pointer].clone()).throw()
    }

    fn retrieve_heap(&mut self) -> Result<(), VmError> {
        if let Some(addr) = self.stack.pop() {
            if addr < 0 || addr as usize >= self.heap.len() {
                return VmErrorKind::NumberOutOfBoundsError(
                    self.instructions[self.instruction_pointer].clone(),
                    addr,
                    0,
                    self.heap.len() as i32 - 1,
                )
                .throw();
            }

            self.stack.push(self.heap[addr as usize]);

            return Ok(());
        }

        VmErrorKind::StackUnderflow(self.instructions[self.instruction_pointer].clone()).throw()
    }

    fn mark(&mut self) -> Result<(), VmError> {
        Ok(())
    }

    fn call(&mut self) -> Result<(), VmError> {
        if let Instruction::Call(label) = &self.instructions[self.instruction_pointer] {
            self.call_stack.push(self.instruction_pointer);
            self.instruction_pointer = label.index;

            return Ok(());
        }

        unreachable!();
    }

    fn jump(&mut self) -> Result<(), VmError> {
        if let Instruction::Jump(label) = &self.instructions[self.instruction_pointer] {
            self.instruction_pointer = label.index;

            return Ok(());
        }

        unreachable!();
    }

    fn jump_zero(&mut self) -> Result<(), VmError> {
        if let Instruction::JumpZero(label) = &self.instructions[self.instruction_pointer] {
            if let Some(val) = self.stack.pop() {
                if val != 0 {
                    return Ok(());
                }
                self.instruction_pointer = label.index;

                return Ok(());
            }

            return VmErrorKind::StackUnderflow(
                self.instructions[self.instruction_pointer].clone(),
            )
            .throw();
        }

        unreachable!();
    }

    fn jump_negative(&mut self) -> Result<(), VmError> {
        if let Instruction::JumpNegative(label) = &self.instructions[self.instruction_pointer] {
            if let Some(val) = self.stack.pop() {
                if val >= 0 {
                    return Ok(());
                }
                self.instruction_pointer = label.index;

                return Ok(());
            }
            return VmErrorKind::StackUnderflow(
                self.instructions[self.instruction_pointer].clone(),
            )
            .throw();
        }

        unreachable!();
    }

    fn r#return(&mut self) -> Result<(), VmError> {
        if let Some(frame) = self.call_stack.pop() {
            self.instruction_pointer = frame;

            return Ok(());
        }

        VmErrorKind::StackUnderflow(self.instructions[self.instruction_pointer].clone()).throw()
    }

    fn exit(&mut self) -> Result<(), VmError> {
        self.done = true;

        Ok(())
    }

    fn out_char(&mut self) -> Result<(), VmError> {
        if let Some(character) = self.stack.pop() {
            if character < 0 {
                return VmErrorKind::NumberOutOfBoundsError(
                    self.instructions[self.instruction_pointer].clone(),
                    character,
                    0,
                    i32::MAX,
                )
                .throw();
            }

            if self.config.suppress_output {
                return Ok(());
            }

            if let Some(character) = char::from_u32(character as u32) {
                match write!(stdout(), "{}", character) {
                    Ok(val) => val,
                    Err(_) => {
                        return VmErrorKind::IOError(
                            self.instructions[self.instruction_pointer].clone(),
                        )
                        .throw()
                    }
                };
                match stdout().flush() {
                    Ok(val) => val,
                    Err(_) => {
                        return VmErrorKind::IOError(
                            self.instructions[self.instruction_pointer].clone(),
                        )
                        .throw()
                    }
                };

                return Ok(());
            }
        }

        VmErrorKind::StackUnderflow(self.instructions[self.instruction_pointer].clone()).throw()
    }

    fn out_int(&mut self) -> Result<(), VmError> {
        if let Some(num) = self.stack.pop() {
            if self.config.suppress_output {
                return Ok(());
            }
            match write!(stdout(), "{}", num) {
                Ok(val) => val,
                Err(_) => {
                    return VmErrorKind::IOError(
                        self.instructions[self.instruction_pointer].clone(),
                    )
                    .throw()
                }
            };
            match stdout().flush() {
                Ok(val) => val,
                Err(_) => {
                    return VmErrorKind::IOError(
                        self.instructions[self.instruction_pointer].clone(),
                    )
                    .throw()
                }
            };

            return Ok(());
        }

        VmErrorKind::StackUnderflow(self.instructions[self.instruction_pointer].clone()).throw()
    }

    fn read_char(&mut self) -> Result<(), VmError> {
        #[cfg(target_arch = "wasm32")]
        unimplemented!();
        #[cfg(not(target_arch = "wasm32"))]
        if let Some(addr) = self.stack.pop() {
            if addr < 0 || addr as usize >= self.heap.len() {
                return VmErrorKind::NumberOutOfBoundsError(
                    self.instructions[self.instruction_pointer].clone(),
                    addr,
                    0,
                    i32::MAX,
                )
                .throw();
            }

            match stdout().flush() {
                Ok(val) => val,
                Err(_) => {
                    return VmErrorKind::IOError(
                        self.instructions[self.instruction_pointer].clone(),
                    )
                    .throw()
                }
            };
            return match Getch::new().getch() {
                Ok(val) => {
                    self.heap[addr as usize] = val as i32;
                    match write!(stdout(), "{}", char::from_u32(val as u32).unwrap()) {
                        Ok(val) => val,
                        Err(_) => {
                            return VmErrorKind::IOError(
                                self.instructions[self.instruction_pointer].clone(),
                            )
                            .throw()
                        }
                    };
                    match stdout().flush() {
                        Ok(val) => val,
                        Err(_) => {
                            return VmErrorKind::IOError(
                                self.instructions[self.instruction_pointer].clone(),
                            )
                            .throw()
                        }
                    };

                    Ok(())
                }
                Err(_) => {
                    return VmErrorKind::IOError(
                        self.instructions[self.instruction_pointer].clone(),
                    )
                    .throw()
                }
            };
        }

        VmErrorKind::StackUnderflow(self.instructions[self.instruction_pointer].clone()).throw()
    }

    fn read_int(&mut self) -> Result<(), VmError> {
        if let Some(addr) = self.stack.pop() {
            if addr < 0 || addr as usize >= self.heap.len() {
                return VmErrorKind::NumberOutOfBoundsError(
                    self.instructions[self.instruction_pointer].clone(),
                    addr,
                    0,
                    self.heap.len() as i32 - 1,
                )
                .throw();
            }
            match stdout().flush() {
                Ok(val) => val,
                Err(_) => {
                    return VmErrorKind::IOError(
                        self.instructions[self.instruction_pointer].clone(),
                    )
                    .throw()
                }
            };
            let mut input_text = String::new();
            match stdin().read_line(&mut input_text) {
                Ok(val) => val,
                Err(_) => {
                    return VmErrorKind::IOError(
                        self.instructions[self.instruction_pointer].clone(),
                    )
                    .throw()
                }
            };

            let trimmed = input_text.trim();
            let num = match trimmed.parse::<i32>() {
                Ok(val) => val,
                Err(_) => {
                    return VmErrorKind::IOError(
                        self.instructions[self.instruction_pointer].clone(),
                    )
                    .throw()
                }
            };
            self.heap[addr as usize] = num;

            return Ok(());
        }

        VmErrorKind::IOError(self.instructions[self.instruction_pointer].clone()).throw()
    }

    pub fn exec(&mut self) -> Result<(), VmError> {
        if self.config.debug {
            dbg!(&self.stack);
            dbg!(&self.call_stack);
            dbg!(&self.instruction_pointer);
            dbg!(&self.instructions[self.instruction_pointer]);
        }
        if self.config.debug_heap {
            dbg!(self.generate_debug_heap_dump());
        }
        let res = match self.instructions[self.instruction_pointer] {
            Instruction::PushStack(_) => self.push_stack(),
            Instruction::DuplicateStack => self.duplicate_stack(),
            Instruction::CopyNthStack(_) => self.copy_nth_stack(),
            Instruction::SwapStack => self.swap_stack(),
            Instruction::DiscardStack => self.discard_stack(),
            Instruction::SlideNStack(_) => self.slide_n_stack(),
            Instruction::Add => self.add(),
            Instruction::Subtract => self.subtract(),
            Instruction::Multiply => self.multiply(),
            Instruction::IntegerDivision => self.integer_division(),
            Instruction::Modulo => self.modulo(),
            Instruction::StoreHeap => self.store_heap(),
            Instruction::RetrieveHeap => self.retrieve_heap(),
            Instruction::Mark(_) => self.mark(),
            Instruction::Call(_) => self.call(),
            Instruction::Jump(_) => self.jump(),
            Instruction::JumpZero(_) => self.jump_zero(),
            Instruction::JumpNegative(_) => self.jump_negative(),
            Instruction::Return => self.r#return(),
            Instruction::Exit => self.exit(),
            Instruction::OutCharacter => self.out_char(),
            Instruction::OutInteger => self.out_int(),
            Instruction::ReadCharacter => self.read_char(),
            Instruction::ReadInteger => self.read_int(),
        };

        self.instruction_pointer += 1;

        res
    }
}

#[cfg(test)]
mod tests {
    use super::{SourceType, Vm, VmConfig, VmError};

    #[test]
    fn interpret_stack() -> Result<(), VmError> {
        let config =
            VmConfig::default_no_heap_suppressed("ws/interpret_stack.ws", SourceType::Whitespace);
        let mut interpreter = Vm::new(config)?;

        interpreter.run()?;

        assert_eq!(interpreter.stack, vec![-1]);
        assert!(interpreter.heap.is_empty());

        Ok(())
    }

    #[test]
    fn interpret_arithmetic() -> Result<(), VmError> {
        let config = VmConfig::default_no_heap_suppressed(
            "ws/interpret_arithmetic.ws",
            SourceType::Whitespace,
        );
        let mut interpreter = Vm::new(config)?;

        interpreter.run()?;

        assert_eq!(interpreter.stack, vec![4]);
        assert!(interpreter.heap.is_empty());

        Ok(())
    }

    #[test]
    fn interpret_heap() -> Result<(), VmError> {
        let config =
            VmConfig::default_heap_suppressed("ws/interpret_heap.ws", SourceType::Whitespace);
        let mut interpreter = Vm::new(config)?;

        interpreter.run()?;

        assert_eq!(interpreter.stack, vec![-8, 10]);

        Ok(())
    }

    #[test]
    fn interpret_flow() -> Result<(), VmError> {
        let config =
            VmConfig::default_no_heap_suppressed("ws/interpret_flow.ws", SourceType::Whitespace);
        let mut interpreter = Vm::new(config)?;

        interpreter.run()?;
        assert_eq!(interpreter.stack, Vec::<i32>::new());

        Ok(())
    }

    #[test]
    fn interpret_io() -> Result<(), VmError> {
        let config =
            VmConfig::default_no_heap_suppressed("ws/interpret_io.ws", SourceType::Whitespace);
        let mut interpreter = Vm::new(config)?;

        interpreter.run()?;

        Ok(())
    }
}
