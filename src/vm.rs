use crate::parser::ParseError;
use crate::ws::{WsCommandKind, WsImpKind, WsParamKind};
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
    /// - `source` the whitespace source as a String
    /// - `heap_size` the size of the heap address space (each address holds an i32)
    /// - `raw` print the raw instructions of the parsed source file to stdout
    /// - `debug` print debugging information to stdout when executing an instruction
    /// - `debug_heap` print heap dump to stdout when executing an instruction
    #[wasm_bindgen(constructor)]
    pub fn new(
        source: &str,
        heap_size: usize,
        raw: bool,
        debug: bool,
        debug_heap: bool,
        suppress_output: bool,
    ) -> VmConfig {
        VmConfig {
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
    /// - `source` the whitespace source as a String
    pub fn default_heap(source: &str) -> VmConfig {
        VmConfig {
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
    /// - `source` the whitespace source as a String
    pub fn default_no_heap(source: &str) -> VmConfig {
        VmConfig {
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
    /// - `source` the whitespace source as a String
    pub fn default_heap_suppressed(source: &str) -> VmConfig {
        VmConfig {
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
    /// - `source` the whitespace source as a String
    pub fn default_no_heap_suppressed(source: &str) -> VmConfig {
        VmConfig {
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
    /// - `source` the whitespace source as a String
    pub fn debug_heap(source: &str) -> VmConfig {
        VmConfig {
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
    /// - `source` the whitespace source as a String
    pub fn debug_no_heap(source: &str) -> VmConfig {
        VmConfig {
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
    /// - `source` the whitespace source as a String
    pub fn raw(source: &str) -> VmConfig {
        VmConfig {
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
    /// - `file_name` the path to the whitespace source file on disk
    /// - `heap_size` the size of the heap address space (each address holds an i32)
    /// - `raw` print the IR of the parsed source file to stdout
    /// - `debug` print debugging information to stdout when executing an instruction
    /// - `debug_heap` print heap dump to stdout when executing an instruction
    pub fn new(
        file_name: &str,
        heap_size: usize,
        raw: bool,
        debug: bool,
        debug_heap: bool,
        suppress_output: bool,
    ) -> VmConfig {
        VmConfig {
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
    /// `file_name` - the name of the source file on disk
    pub fn default_heap(file_name: &str) -> VmConfig {
        VmConfig {
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
    /// `file_name` - the name of the source file on disk
    pub fn default_no_heap(file_name: &str) -> VmConfig {
        VmConfig {
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
    /// `file_name` - the name of the source file on disk
    pub fn default_heap_suppressed(file_name: &str) -> VmConfig {
        VmConfig {
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
    /// `file_name` - the name of the source file on disk
    pub fn default_no_heap_suppressed(file_name: &str) -> VmConfig {
        VmConfig {
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
    /// `file_name` - the name of the source file on disk
    pub fn debug_heap(file_name: &str) -> VmConfig {
        VmConfig {
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
    /// `file_name` - the name of the source file on disk
    pub fn debug_no_heap(file_name: &str) -> VmConfig {
        VmConfig {
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
    /// `file_name` - the name of the source file on disk
    pub fn raw(file_name: &str) -> VmConfig {
        VmConfig {
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
    ParseLogicError(Instruction),
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
            VmErrorKind::ParseLogicError(instr) => format!("the parser delivered an inconsistent state, something is severely broken from an application logic point of view. in other words: engineer fucked up. if you receive this error message please make sure to report this as an issue (please also supply the whitespace source) over at https://github.com/d3psi/spacey/issues. thank you. issue occurred when attempting to execute: {:?}", instr),
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
        let mut parser = match WsParser::new(source_or_source_file) {
            Ok(content) => content,
            Err(err) => return VmErrorKind::ParseError(err).throw(),
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
                    labels.insert(label, i);
                }
                _ => (),
            }
        }

        for instr in &mut instructions {
            if let Some(WsParamKind::Label(label, _)) = instr.param.clone() {
                if let Some(index) = labels.get(&label) {
                    instr.param = Some(WsParamKind::Label(label, *index));
                }
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
        while let Some(ip) = self.next_instruction() {
            self.exec(ip)?;
        }

        let last = &self.instructions[self.instruction_pointer - 1];
        if last.cmd != WsCommandKind::Exit {
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

    fn stack(&mut self, ip: usize) -> Result<(), VmError> {
        let instr = &self.instructions[ip];
        match instr.cmd {
            WsCommandKind::PushStack => {
                if let Some(WsParamKind::Number(val)) = instr.param {
                    self.stack.push(val);

                    return Ok(());
                }

                VmErrorKind::ParseLogicError(instr.clone()).throw()
            }
            WsCommandKind::DuplicateStack => {
                if let Some(val) = self.stack.pop() {
                    self.stack.push(val);
                    self.stack.push(val);

                    return Ok(());
                }

                VmErrorKind::StackUnderflow(instr.clone()).throw()
            }
            WsCommandKind::CopyNthStack => {
                if let Some(WsParamKind::Number(addr)) = instr.param {
                    if addr < 0 || addr as usize >= self.stack.len() {
                        return VmErrorKind::NumberOutOfBoundsError(
                            instr.clone(),
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

                VmErrorKind::ParseLogicError(instr.clone()).throw()
            }
            WsCommandKind::SwapStack => {
                if let Some(val) = self.stack.pop() {
                    if let Some(other) = self.stack.pop() {
                        self.stack.push(val);
                        self.stack.push(other);

                        return Ok(());
                    }

                    return VmErrorKind::StackUnderflow(instr.clone()).throw();
                }
                VmErrorKind::StackUnderflow(instr.clone()).throw()
            }
            WsCommandKind::DiscardStack => {
                if self.stack.pop().is_some() {
                    return Ok(());
                }

                VmErrorKind::StackUnderflow(instr.clone()).throw()
            }
            WsCommandKind::SlideNStack => {
                if let Some(top) = self.stack.pop() {
                    if let Some(WsParamKind::Number(val)) = instr.param {
                        if val < 0 {
                            return VmErrorKind::NumberOutOfBoundsError(
                                instr.clone(),
                                val,
                                0,
                                i32::MAX,
                            )
                            .throw();
                        }
                        for _i in 0..val {
                            self.stack.pop();
                        }
                        self.stack.push(top);

                        return Ok(());
                    }

                    return VmErrorKind::ParseLogicError(instr.clone()).throw();
                }

                VmErrorKind::StackUnderflow(instr.clone()).throw()
            }
            _ => VmErrorKind::ParseLogicError(instr.clone()).throw(),
        }
    }

    fn arithmetic(&mut self, ip: usize) -> Result<(), VmError> {
        let instr = &self.instructions[ip];
        match instr.cmd {
            WsCommandKind::Add => {
                if let Some(right) = self.stack.pop() {
                    if let Some(left) = self.stack.pop() {
                        self.stack.push(left + right);

                        return Ok(());
                    }
                }

                VmErrorKind::StackUnderflow(instr.clone()).throw()
            }
            WsCommandKind::Subtract => {
                if let Some(right) = self.stack.pop() {
                    if let Some(left) = self.stack.pop() {
                        self.stack.push(left - right);

                        return Ok(());
                    }
                }

                VmErrorKind::StackUnderflow(instr.clone()).throw()
            }
            WsCommandKind::Multiply => {
                if let Some(right) = self.stack.pop() {
                    if let Some(left) = self.stack.pop() {
                        self.stack.push(left * right);

                        return Ok(());
                    }
                }

                VmErrorKind::StackUnderflow(instr.clone()).throw()
            }
            WsCommandKind::IntegerDivision => {
                if let Some(right) = self.stack.pop() {
                    if let Some(left) = self.stack.pop() {
                        self.stack.push(left / right);

                        return Ok(());
                    }
                }

                VmErrorKind::StackUnderflow(instr.clone()).throw()
            }
            WsCommandKind::Modulo => {
                if let Some(right) = self.stack.pop() {
                    if let Some(left) = self.stack.pop() {
                        self.stack.push(left % right);

                        return Ok(());
                    }
                }

                VmErrorKind::StackUnderflow(instr.clone()).throw()
            }
            _ => VmErrorKind::ParseLogicError(instr.clone()).throw(),
        }
    }

    fn heap(&mut self, ip: usize) -> Result<(), VmError> {
        let instr = &self.instructions[ip];
        match instr.cmd {
            WsCommandKind::StoreHeap => {
                if let Some(val) = self.stack.pop() {
                    if let Some(addr) = self.stack.pop() {
                        if addr < 0 || addr as usize >= self.heap.len() {
                            return VmErrorKind::NumberOutOfBoundsError(
                                instr.clone(),
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

                VmErrorKind::StackUnderflow(instr.clone()).throw()
            }
            WsCommandKind::RetrieveHeap => {
                if let Some(addr) = self.stack.pop() {
                    if addr < 0 || addr as usize >= self.heap.len() {
                        return VmErrorKind::NumberOutOfBoundsError(
                            instr.clone(),
                            addr,
                            0,
                            self.heap.len() as i32 - 1,
                        )
                        .throw();
                    }

                    self.stack.push(self.heap[addr as usize]);

                    return Ok(());
                }

                VmErrorKind::StackUnderflow(instr.clone()).throw()
            }
            _ => VmErrorKind::ParseLogicError(instr.clone()).throw(),
        }
    }

    fn flow(&mut self, ip: usize) -> Result<(), VmError> {
        let instr = &self.instructions[ip];
        match instr.cmd {
            WsCommandKind::Mark => Ok(()),
            WsCommandKind::Call => {
                if let Some(WsParamKind::Label(_, index)) = &instr.param {
                    self.call_stack.push(self.instruction_pointer);
                    self.instruction_pointer = *index;

                    return Ok(());
                }

                VmErrorKind::ParseLogicError(instr.clone()).throw()
            }
            WsCommandKind::Jump => {
                if let Some(WsParamKind::Label(_, index)) = &instr.param {
                    self.instruction_pointer = *index;

                    return Ok(());
                }

                VmErrorKind::ParseLogicError(instr.clone()).throw()
            }
            WsCommandKind::JumpZero => {
                if let Some(val) = self.stack.pop() {
                    if val != 0 {
                        return Ok(());
                    }
                    if let Some(WsParamKind::Label(_, index)) = &instr.param {
                        self.instruction_pointer = *index;

                        return Ok(());
                    }
                    return VmErrorKind::StackUnderflow(instr.clone()).throw();
                }

                VmErrorKind::ParseLogicError(instr.clone()).throw()
            }
            WsCommandKind::JumpNegative => {
                if let Some(val) = self.stack.pop() {
                    if val >= 0 {
                        return Ok(());
                    }
                    if let Some(WsParamKind::Label(_, index)) = &instr.param {
                        self.instruction_pointer = *index;

                        return Ok(());
                    }

                    return VmErrorKind::StackUnderflow(instr.clone()).throw();
                }

                VmErrorKind::ParseLogicError(instr.clone()).throw()
            }
            WsCommandKind::Return => {
                if let Some(frame) = self.call_stack.pop() {
                    self.instruction_pointer = frame;

                    return Ok(());
                }

                VmErrorKind::StackUnderflow(instr.clone()).throw()
            }
            WsCommandKind::Exit => {
                self.done = true;

                Ok(())
            }
            _ => VmErrorKind::ParseLogicError(instr.clone()).throw(),
        }
    }

    fn io(&mut self, ip: usize) -> Result<(), VmError> {
        let instr = &self.instructions[ip];
        match instr.cmd {
            WsCommandKind::OutCharacter => {
                if let Some(character) = self.stack.pop() {
                    if character < 0 {
                        return VmErrorKind::NumberOutOfBoundsError(
                            instr.clone(),
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
                            Err(_) => return VmErrorKind::IOError(instr.clone()).throw(),
                        };
                        match stdout().flush() {
                            Ok(val) => val,
                            Err(_) => return VmErrorKind::IOError(instr.clone()).throw(),
                        };

                        return Ok(());
                    }
                }

                VmErrorKind::StackUnderflow(instr.clone()).throw()
            }
            WsCommandKind::OutInteger => {
                if let Some(number) = self.stack.pop() {
                    if self.config.suppress_output {
                        return Ok(());
                    }
                    match write!(stdout(), "{}", number) {
                        Ok(val) => val,
                        Err(_) => return VmErrorKind::IOError(instr.clone()).throw(),
                    };
                    match stdout().flush() {
                        Ok(val) => val,
                        Err(_) => return VmErrorKind::IOError(instr.clone()).throw(),
                    };

                    return Ok(());
                }

                VmErrorKind::StackUnderflow(instr.clone()).throw()
            }
            WsCommandKind::ReadCharacter => {
                #[cfg(target_arch = "wasm32")]
                unimplemented!();
                #[cfg(not(target_arch = "wasm32"))]
                if let Some(addr) = self.stack.pop() {
                    if addr < 0 || addr as usize >= self.heap.len() {
                        return VmErrorKind::NumberOutOfBoundsError(
                            instr.clone(),
                            addr,
                            0,
                            self.heap.len() as i32 - 1,
                        )
                        .throw();
                    }

                    match stdout().flush() {
                        Ok(val) => val,
                        Err(_) => return VmErrorKind::IOError(instr.clone()).throw(),
                    };
                    return match Getch::new().getch() {
                        Ok(val) => {
                            self.heap[addr as usize] = val as i32;
                            match write!(stdout(), "{}", char::from_u32(val as u32).unwrap()) {
                                Ok(val) => val,
                                Err(_) => return VmErrorKind::IOError(instr.clone()).throw(),
                            };
                            match stdout().flush() {
                                Ok(val) => val,
                                Err(_) => return VmErrorKind::IOError(instr.clone()).throw(),
                            };

                            Ok(())
                        }
                        Err(_) => VmErrorKind::IOError(instr.clone()).throw(),
                    };
                }

                VmErrorKind::StackUnderflow(instr.clone()).throw()
            }
            WsCommandKind::ReadInteger => {
                if let Some(addr) = self.stack.pop() {
                    if addr < 0 || addr as usize >= self.heap.len() {
                        return VmErrorKind::NumberOutOfBoundsError(
                            instr.clone(),
                            addr,
                            0,
                            self.heap.len() as i32 - 1,
                        )
                        .throw();
                    }
                    match stdout().flush() {
                        Ok(val) => val,
                        Err(_) => return VmErrorKind::IOError(instr.clone()).throw(),
                    };
                    let mut input_text = String::new();
                    match stdin().read_line(&mut input_text) {
                        Ok(val) => val,
                        Err(_) => return VmErrorKind::IOError(instr.clone()).throw(),
                    };

                    let trimmed = input_text.trim();
                    let num = match trimmed.parse::<i32>() {
                        Ok(val) => val,
                        Err(_) => return VmErrorKind::IOError(instr.clone()).throw(),
                    };
                    self.heap[addr as usize] = num;

                    return Ok(());
                }

                VmErrorKind::IOError(instr.clone()).throw()
            }
            _ => VmErrorKind::ParseLogicError(instr.clone()).throw(),
        }
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

    /// Executes a single instruction in the interpreter
    ///
    /// `instr` - the instruction to execute
    pub fn exec(&mut self, ip: usize) -> Result<(), VmError> {
        if self.config.debug {
            dbg!(&self.stack);
            dbg!(&self.call_stack);
            dbg!(&self.instruction_pointer);
            dbg!(&self.instructions[self.instruction_pointer]);
        }
        if self.config.debug_heap {
            dbg!(self.generate_debug_heap_dump());
        }
        let res = match self.instructions[ip].imp {
            WsImpKind::Stack => self.stack(ip),
            WsImpKind::Arithmetic => self.arithmetic(ip),
            WsImpKind::Heap => self.heap(ip),
            WsImpKind::Flow => self.flow(ip),
            WsImpKind::IO => self.io(ip),
        };

        self.instruction_pointer += 1;

        res
    }
}

#[cfg(test)]
mod tests {
    use super::{VmConfig, VmError, WsVm};

    #[test]
    fn interpret_stack() -> Result<(), VmError> {
        let config = VmConfig::default_no_heap_suppressed("ws/interpret_stack.ws");
        let mut interpreter = WsVm::new(config)?;

        interpreter.run()?;

        assert_eq!(interpreter.stack, vec![-1]);
        assert!(interpreter.heap.is_empty());

        Ok(())
    }

    #[test]
    fn interpret_arithmetic() -> Result<(), VmError> {
        let config = VmConfig::default_no_heap_suppressed("ws/interpret_arithmetic.ws");
        let mut interpreter = WsVm::new(config)?;

        interpreter.run()?;

        assert_eq!(interpreter.stack, vec![4]);
        assert!(interpreter.heap.is_empty());

        Ok(())
    }

    #[test]
    fn interpret_heap() -> Result<(), VmError> {
        let config = VmConfig::default_heap_suppressed("ws/interpret_heap.ws");
        let mut interpreter = WsVm::new(config)?;

        interpreter.run()?;

        assert_eq!(interpreter.stack, vec![-8, 10]);

        Ok(())
    }

    #[test]
    fn interpret_flow() -> Result<(), VmError> {
        let config = VmConfig::default_no_heap_suppressed("ws/interpret_flow.ws");
        let mut interpreter = WsVm::new(config)?;

        interpreter.run()?;
        assert_eq!(interpreter.stack, vec![]);

        Ok(())
    }

    #[test]
    fn interpret_io() -> Result<(), VmError> {
        let config = VmConfig::default_no_heap_suppressed("ws/interpret_io.ws");
        let mut interpreter = WsVm::new(config)?;

        interpreter.run()?;

        Ok(())
    }
}
