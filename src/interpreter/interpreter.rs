use crate::parser::parser::ParseError;
use crate::parser::{CommandKind, ImpKind, ParamKind};
use crate::{Instruction, Parser};
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
pub struct Interpreter {
    config: InterpreterConfig,
    stack: Vec<i32>,
    call_stack: Vec<usize>,
    heap: Vec<i32>,
    instruction_pointer: usize,
    instructions: Vec<Instruction>,
    done: bool,
}

/// Configuration options for the interpreter
#[wasm_bindgen]
pub struct InterpreterConfig {
    #[cfg(not(target_arch = "wasm32"))]
    file_name: String,
    #[cfg(target_arch = "wasm32")]
    source: String,
    heap_size: usize,
    ir: bool,
    debug: bool,
    debug_heap: bool,
    suppress_output: bool,
}

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
impl InterpreterConfig {
    /// Creates a new interpreter config with the given arguments
    ///
    /// - `source` the whitespace source as a String
    /// - `heap_size` the size of the heap address space (each address holds an i32)
    /// - `ir` print the IR of the parsed source file to stdout
    /// - `debug` print debugging information to stdout when executing an instruction
    /// - `debug_heap` print heap dump to stdout when executing an instruction
    pub fn from_source(
        source: &str,
        heap_size: usize,
        ir: bool,
        debug: bool,
        debug_heap: bool,
        suppress_output: bool,
    ) -> InterpreterConfig {
        InterpreterConfig {
            source: source.to_string(),
            heap_size,
            ir,
            debug,
            debug_heap,
            suppress_output,
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
impl InterpreterConfig {
    /// Creates a new interpreter config with the given arguments
    ///
    /// - `file_name` the path to the whitespace source file on disk
    /// - `heap_size` the size of the heap address space (each address holds an i32)
    /// - `ir` print the IR of the parsed source file to stdout
    /// - `debug` print debugging information to stdout when executing an instruction
    /// - `debug_heap` print heap dump to stdout when executing an instruction
    pub fn new(
        file_name: &str,
        heap_size: usize,
        ir: bool,
        debug: bool,
        debug_heap: bool,
        suppress_output: bool,
    ) -> InterpreterConfig {
        InterpreterConfig {
            file_name: file_name.to_string(),
            heap_size,
            ir,
            debug,
            debug_heap,
            suppress_output,
        }
    }

    /// Returns a default interpreter configuration with the default heap size
    ///
    /// `file_name` - the name of the source file on disk
    pub fn default_heap(file_name: &str) -> InterpreterConfig {
        InterpreterConfig {
            file_name: file_name.to_string(),
            heap_size: DEFAULT_HEAP_SIZE,
            ir: false,
            debug: false,
            debug_heap: false,
            suppress_output: false,
        }
    }

    /// Returns a default interpreter configuration with no heap
    ///
    /// `file_name` - the name of the source file on disk
    pub fn default_no_heap(file_name: &str) -> InterpreterConfig {
        InterpreterConfig {
            file_name: file_name.to_string(),
            heap_size: 0,
            ir: false,
            debug: false,
            debug_heap: false,
            suppress_output: false,
        }
    }

    /// Returns a default interpreter configuration with the default heap size, suppressing output
    ///
    /// `file_name` - the name of the source file on disk
    pub fn default_heap_suppressed(file_name: &str) -> InterpreterConfig {
        InterpreterConfig {
            file_name: file_name.to_string(),
            heap_size: DEFAULT_HEAP_SIZE,
            ir: false,
            debug: false,
            debug_heap: false,
            suppress_output: true,
        }
    }

    /// Returns a default interpreter configuration with no heap, suppressing output
    ///
    /// `file_name` - the name of the source file on disk
    pub fn default_no_heap_suppressed(file_name: &str) -> InterpreterConfig {
        InterpreterConfig {
            file_name: file_name.to_string(),
            heap_size: 0,
            ir: false,
            debug: false,
            debug_heap: false,
            suppress_output: true,
        }
    }

    /// Returns a default debug interpreter configuration with the default heap size
    ///
    /// `file_name` - the name of the source file on disk
    pub fn debug_heap(file_name: &str) -> InterpreterConfig {
        InterpreterConfig {
            file_name: file_name.to_string(),
            heap_size: DEFAULT_HEAP_SIZE,
            ir: false,
            debug: true,
            debug_heap: true,
            suppress_output: false,
        }
    }

    /// Returns a default debug interpreter configuration with no heap
    ///
    /// `file_name` - the name of the source file on disk
    pub fn debug_no_heap(file_name: &str) -> InterpreterConfig {
        InterpreterConfig {
            file_name: file_name.to_string(),
            heap_size: 0,
            ir: false,
            debug: true,
            debug_heap: false,
            suppress_output: false,
        }
    }

    /// Returns a default debug interpreter configuration to only compute the intermediate
    /// representation of the source
    ///
    /// `file_name` - the name of the source file on disk
    pub fn ir(file_name: &str) -> InterpreterConfig {
        InterpreterConfig {
            file_name: file_name.to_string(),
            heap_size: 0,
            ir: true,
            debug: false,
            debug_heap: false,
            suppress_output: false,
        }
    }
}

#[derive(Debug)]
enum InterpretErrorKind {
    ParseError(ParseError),
    ParseLogicError(Instruction),
    StackUnderflow(Instruction),
    NumberOutOfBoundsError(Instruction, i32, i32, i32),
    NoTermination(Instruction),
    IOError(Instruction),
}

impl Display for InterpretErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl InterpretErrorKind {
    fn throw<T>(self) -> Result<T, InterpretError> {
        let msg = match &self {
            InterpretErrorKind::ParseLogicError(instr) => format!("the parser delivered an inconsistent state, something is severely broken from an application logic point of view. in other words: engineer fucked up. if you receive this error message please make sure to report this as an issue (please also supply the whitespace source) over at https://github.com/d3psi/spacey/issues. thank you. issue occurred when attempting to execute: {:?}", instr),
            InterpretErrorKind::StackUnderflow(instr) => format!("stack is empty - failed executing: {:?}", instr),
            InterpretErrorKind::NumberOutOfBoundsError(instr, num, low, high) => format!("number is out of bounds for: {:?}, expected in the closed interval bounded by {} and {}, but was {}", instr, low, high, num),
            InterpretErrorKind::NoTermination(instr) => format!("no termination instruction after last executed instruction: {:?}", instr),
            InterpretErrorKind::IOError(instr) => format!("stdin error when executing: {:?}", instr),
            InterpretErrorKind::ParseError(err) => format!("parse error occurred: {}, {}", err.kind, err.msg)
        };
        Err(InterpretError { msg, kind: self })
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct InterpretError {
    msg: String,
    kind: InterpretErrorKind,
}

impl Into<JsValue> for InterpretError {
    fn into(self) -> JsValue {
        JsValue::from(format!("spacey error occured: {}, {}", self.kind, self.msg))
    }
}

impl Display for InterpretError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[wasm_bindgen]
impl Interpreter {
    /// Creates a new interpreter with the given arguments
    ///
    /// - `config` The configuration of the interpreter
    pub fn new(config: InterpreterConfig) -> Result<Interpreter, InterpretError> {
        #[cfg(not(target_arch = "wasm32"))]
        let source_or_source_file = &config.file_name;
        #[cfg(target_arch = "wasm32")]
        let source_or_source_file = &config.source;
        let mut parser = match Parser::new(source_or_source_file) {
            Ok(content) => content,
            Err(err) => return InterpretErrorKind::ParseError(err).throw(),
        };
        let mut instructions = vec![];
        for instr in &mut parser {
            let instr = match instr {
                Ok(content) => content,
                Err(err) => return InterpretErrorKind::ParseError(err).throw(),
            };
            if config.ir {
                dbg!(&instr);
            }
            instructions.push(instr);
        }
        let stack = vec![];
        let call_stack = vec![];
        let heap = vec![0; config.heap_size];
        let mut labels = HashMap::new();
        let instruction_pointer = 0;
        let done = false;

        for (i, instr) in instructions.iter().enumerate() {
            if instr.cmd == CommandKind::Mark {
                if let Some(ParamKind::Label(label, _)) = instr.param.clone() {
                    labels.insert(label, i);
                }
            }
        }

        for instr in &mut instructions {
            if let Some(ParamKind::Label(label, _)) = instr.param.clone() {
                if let Some(index) = labels.get(&label) {
                    instr.param = Some(ParamKind::Label(label, *index));
                }
            }
        }

        Ok(Interpreter {
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
    pub fn next_instruction(&self) -> Option<Instruction> {
        if self.done {
            return None;
        }
        if self.instruction_pointer < self.instructions.len() {
            return Some(self.instructions[self.instruction_pointer].clone());
        }

        None
    }

    /// Executes all instructions - runs the program.
    pub fn run(&mut self) -> Result<(), InterpretError> {
        while let Some(instr) = self.next_instruction() {
            self.exec(instr)?;
        }

        let last = self.instructions[self.instruction_pointer - 1].clone();
        if last.cmd != CommandKind::Exit {
            return InterpretErrorKind::NoTermination(last).throw();
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

    fn stack(&mut self, instr: Instruction) -> Result<(), InterpretError> {
        match instr.cmd {
            CommandKind::PushStack => {
                if let Some(ParamKind::Number(val)) = instr.param {
                    self.stack.push(val);

                    return Ok(());
                }

                InterpretErrorKind::ParseLogicError(instr).throw()
            }
            CommandKind::DuplicateStack => {
                if let Some(val) = self.stack.pop() {
                    self.stack.push(val);
                    self.stack.push(val);

                    return Ok(());
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::CopyNthStack => {
                if let Some(ParamKind::Number(addr)) = instr.param {
                    if addr < 0 || addr as usize >= self.stack.len() {
                        return InterpretErrorKind::NumberOutOfBoundsError(
                            instr,
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

                InterpretErrorKind::ParseLogicError(instr).throw()
            }
            CommandKind::SwapStack => {
                if let Some(val) = self.stack.pop() {
                    if let Some(other) = self.stack.pop() {
                        self.stack.push(val);
                        self.stack.push(other);

                        return Ok(());
                    }

                    return InterpretErrorKind::StackUnderflow(instr).throw();
                }
                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::DiscardStack => {
                if self.stack.pop().is_some() {
                    return Ok(());
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::SlideNStack => {
                if let Some(top) = self.stack.pop() {
                    if let Some(ParamKind::Number(val)) = instr.param {
                        if val < 0 {
                            return InterpretErrorKind::NumberOutOfBoundsError(
                                instr,
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

                    return InterpretErrorKind::ParseLogicError(instr).throw();
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            _ => InterpretErrorKind::ParseLogicError(instr).throw(),
        }
    }

    fn arithmetic(&mut self, instr: Instruction) -> Result<(), InterpretError> {
        match instr.cmd {
            CommandKind::Add => {
                if let Some(right) = self.stack.pop() {
                    if let Some(left) = self.stack.pop() {
                        self.stack.push(left + right);

                        return Ok(());
                    }
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::Subtract => {
                if let Some(right) = self.stack.pop() {
                    if let Some(left) = self.stack.pop() {
                        self.stack.push(left - right);

                        return Ok(());
                    }
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::Multiply => {
                if let Some(right) = self.stack.pop() {
                    if let Some(left) = self.stack.pop() {
                        self.stack.push(left * right);

                        return Ok(());
                    }
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::IntegerDivision => {
                if let Some(right) = self.stack.pop() {
                    if let Some(left) = self.stack.pop() {
                        self.stack.push(left / right);

                        return Ok(());
                    }
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::Modulo => {
                if let Some(right) = self.stack.pop() {
                    if let Some(left) = self.stack.pop() {
                        self.stack.push(left % right);

                        return Ok(());
                    }
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            _ => InterpretErrorKind::ParseLogicError(instr).throw(),
        }
    }

    fn heap(&mut self, instr: Instruction) -> Result<(), InterpretError> {
        match instr.cmd {
            CommandKind::StoreHeap => {
                if let Some(val) = self.stack.pop() {
                    if let Some(addr) = self.stack.pop() {
                        if addr < 0 || addr as usize >= self.heap.len() {
                            return InterpretErrorKind::NumberOutOfBoundsError(
                                instr,
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

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::RetrieveHeap => {
                if let Some(addr) = self.stack.pop() {
                    if addr < 0 || addr as usize >= self.heap.len() {
                        return InterpretErrorKind::NumberOutOfBoundsError(
                            instr,
                            addr,
                            0,
                            self.heap.len() as i32 - 1,
                        )
                        .throw();
                    }

                    self.stack.push(self.heap[addr as usize]);

                    return Ok(());
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            _ => InterpretErrorKind::ParseLogicError(instr).throw(),
        }
    }

    fn flow(&mut self, instr: Instruction) -> Result<(), InterpretError> {
        match instr.cmd {
            CommandKind::Mark => Ok(()),
            CommandKind::Call => {
                if let Some(ParamKind::Label(_, index)) = &instr.param {
                    self.call_stack.push(self.instruction_pointer);
                    self.instruction_pointer = *index;

                    return Ok(());
                }

                InterpretErrorKind::ParseLogicError(instr).throw()
            }
            CommandKind::Jump => {
                if let Some(ParamKind::Label(_, index)) = &instr.param {
                    self.instruction_pointer = *index;

                    return Ok(());
                }

                InterpretErrorKind::ParseLogicError(instr).throw()
            }
            CommandKind::JumpZero => {
                if let Some(val) = self.stack.pop() {
                    if val != 0 {
                        return Ok(());
                    }
                    if let Some(ParamKind::Label(_, index)) = &instr.param {
                        self.instruction_pointer = *index;

                        return Ok(());
                    }
                    return InterpretErrorKind::StackUnderflow(instr).throw();
                }

                InterpretErrorKind::ParseLogicError(instr).throw()
            }
            CommandKind::JumpNegative => {
                if let Some(val) = self.stack.pop() {
                    if val >= 0 {
                        return Ok(());
                    }
                    if let Some(ParamKind::Label(_, index)) = &instr.param {
                        self.instruction_pointer = *index;

                        return Ok(());
                    }

                    return InterpretErrorKind::StackUnderflow(instr).throw();
                }

                InterpretErrorKind::ParseLogicError(instr).throw()
            }
            CommandKind::Return => {
                if let Some(frame) = self.call_stack.pop() {
                    self.instruction_pointer = frame;

                    return Ok(());
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::Exit => {
                self.done = true;

                Ok(())
            }
            _ => InterpretErrorKind::ParseLogicError(instr).throw(),
        }
    }

    fn io(&mut self, instr: Instruction) -> Result<(), InterpretError> {
        match instr.cmd {
            CommandKind::OutCharacter => {
                if let Some(character) = self.stack.pop() {
                    if character < 0 {
                        return InterpretErrorKind::NumberOutOfBoundsError(
                            instr,
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
                            Err(_) => return InterpretErrorKind::IOError(instr).throw(),
                        };
                        match stdout().flush() {
                            Ok(val) => val,
                            Err(_) => return InterpretErrorKind::IOError(instr).throw(),
                        };

                        return Ok(());
                    }
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::OutInteger => {
                if let Some(number) = self.stack.pop() {
                    if self.config.suppress_output {
                        return Ok(());
                    }
                    match write!(stdout(), "{}", number) {
                        Ok(val) => val,
                        Err(_) => return InterpretErrorKind::IOError(instr).throw(),
                    };
                    match stdout().flush() {
                        Ok(val) => val,
                        Err(_) => return InterpretErrorKind::IOError(instr).throw(),
                    };

                    return Ok(());
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::ReadCharacter => {
                #[cfg(target_arch = "wasm32")]
                unimplemented!();
                #[cfg(not(target_arch = "wasm32"))]
                if let Some(addr) = self.stack.pop() {
                    if addr < 0 || addr as usize >= self.heap.len() {
                        return InterpretErrorKind::NumberOutOfBoundsError(
                            instr,
                            addr,
                            0,
                            self.heap.len() as i32 - 1,
                        )
                        .throw();
                    }

                    match stdout().flush() {
                        Ok(val) => val,
                        Err(_) => return InterpretErrorKind::IOError(instr).throw(),
                    };
                    return match Getch::new().getch() {
                        Ok(val) => {
                            self.heap[addr as usize] = val as i32;
                            match write!(stdout(), "{}", char::from_u32(val as u32).unwrap()) {
                                Ok(val) => val,
                                Err(_) => return InterpretErrorKind::IOError(instr).throw(),
                            };
                            match stdout().flush() {
                                Ok(val) => val,
                                Err(_) => return InterpretErrorKind::IOError(instr).throw(),
                            };

                            Ok(())
                        }
                        Err(_) => InterpretErrorKind::IOError(instr).throw(),
                    };
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::ReadInteger => {
                if let Some(addr) = self.stack.pop() {
                    if addr < 0 || addr as usize >= self.heap.len() {
                        return InterpretErrorKind::NumberOutOfBoundsError(
                            instr,
                            addr,
                            0,
                            self.heap.len() as i32 - 1,
                        )
                        .throw();
                    }
                    match stdout().flush() {
                        Ok(val) => val,
                        Err(_) => return InterpretErrorKind::IOError(instr).throw(),
                    };
                    let mut input_text = String::new();
                    match stdin().read_line(&mut input_text) {
                        Ok(val) => val,
                        Err(_) => return InterpretErrorKind::IOError(instr).throw(),
                    };

                    let trimmed = input_text.trim();
                    let num = match trimmed.parse::<i32>() {
                        Ok(val) => val,
                        Err(_) => return InterpretErrorKind::IOError(instr).throw(),
                    };
                    self.heap[addr as usize] = num;

                    return Ok(());
                }

                InterpretErrorKind::IOError(instr).throw()
            }
            _ => InterpretErrorKind::ParseLogicError(instr).throw(),
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
    pub fn exec(&mut self, instr: Instruction) -> Result<(), InterpretError> {
        if self.config.debug {
            dbg!(&self.stack);
            dbg!(&self.call_stack);
            dbg!(&self.instruction_pointer);
            dbg!(&self.instructions[self.instruction_pointer]);
        }
        if self.config.debug_heap {
            dbg!(self.generate_debug_heap_dump());
        }
        let res = match instr.imp {
            ImpKind::Stack => self.stack(instr),
            ImpKind::Arithmetic => self.arithmetic(instr),
            ImpKind::Heap => self.heap(instr),
            ImpKind::Flow => self.flow(instr),
            ImpKind::IO => self.io(instr),
        };

        self.instruction_pointer += 1;

        res
    }
}

#[cfg(test)]
mod tests {
    use super::{InterpretError, Interpreter, InterpreterConfig};

    #[test]
    fn interpret_stack() -> Result<(), InterpretError> {
        let config = InterpreterConfig::default_no_heap_suppressed("ws/interpret_stack.ws");
        let mut interpreter = Interpreter::new(config)?;

        interpreter.run()?;

        assert_eq!(interpreter.stack, vec![-1]);
        assert!(interpreter.heap.is_empty());

        Ok(())
    }

    #[test]
    fn interpret_arithmetic() -> Result<(), InterpretError> {
        let config = InterpreterConfig::default_no_heap_suppressed("ws/interpret_arithmetic.ws");
        let mut interpreter = Interpreter::new(config)?;

        interpreter.run()?;

        assert_eq!(interpreter.stack, vec![4]);
        assert!(interpreter.heap.is_empty());

        Ok(())
    }

    #[test]
    fn interpret_heap() -> Result<(), InterpretError> {
        let config = InterpreterConfig::default_heap_suppressed("ws/interpret_heap.ws");
        let mut interpreter = Interpreter::new(config)?;

        interpreter.run()?;

        assert_eq!(interpreter.stack, vec![-8, 10]);

        Ok(())
    }

    #[test]
    fn interpret_flow() -> Result<(), InterpretError> {
        let config = InterpreterConfig::default_no_heap_suppressed("ws/interpret_flow.ws");
        let mut interpreter = Interpreter::new(config)?;

        interpreter.run()?;
        assert_eq!(interpreter.stack, vec![]);

        Ok(())
    }

    #[test]
    fn interpret_io() -> Result<(), InterpretError> {
        let config = InterpreterConfig::default_no_heap_suppressed("ws/interpret_io.ws");
        let mut interpreter = Interpreter::new(config)?;

        interpreter.run()?;

        Ok(())
    }
}
