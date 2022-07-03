use crate::parse::{CommandKind, ImpKind, ParamKind};
use crate::{Instruction, Parser};
use getch::Getch;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::Display;
use std::io::{stdin, stdout, Write};

/// The root component for the virtual machine
pub struct Interpreter {
    stack: Vec<i32>,
    call_stack: Vec<usize>,
    heap: Vec<i32>,
    labels: HashMap<String, usize>,
    instruction_pointer: usize,
    instructions: Vec<Instruction>,
    debug: bool,
    debug_heap: bool,
    suppress_output: bool,
    done: bool,
}

#[derive(Debug)]
enum InterpretErrorKind {
    ParseLogicError(Instruction),
    StackUnderflow(Instruction),
    NumberOutOfBoundsError(Instruction, i32, i32, i32),
    NoTermination(Instruction),
    UnknownLabel(Instruction),
    StdinError(Instruction),
}

impl Display for InterpretErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl InterpretErrorKind {
    fn throw<T>(self) -> Result<T, Box<dyn Error>> {
        let msg = match &self {
            InterpretErrorKind::ParseLogicError(instr) => format!("the parser delivered an inconsistent state, something is severely broken from an application logic point of view. in other words: engineer fucked up. if you receive this error message please make sure to report this as an issue (please also supply the whitespace source) over at https://github.com/d3psi/spacey/issues. thank you. issue occurred when attempting to execute: {:?}", instr),
            InterpretErrorKind::StackUnderflow(instr) => format!("stack is empty - failed executing: {:?}", instr),
            InterpretErrorKind::NumberOutOfBoundsError(instr, num, low, high) => format!("number is out of bounds for: {:?}, expected in the closed interval bounded by {} and {}, but was {}", instr, low, high, num),
            InterpretErrorKind::NoTermination(instr) => format!("no termination instruction after last executed instruction: {:?}", instr),
            InterpretErrorKind::UnknownLabel(instr) => format!("label is not defined, failing instruction: {:?}", instr),
            InterpretErrorKind::StdinError(instr) => format!("stdin error when executing: {:?}", instr)
        };
        Err(Box::new(InterpretError { msg, kind: self }))
    }
}

#[derive(Debug)]
#[allow(dead_code)]
struct InterpretError {
    msg: String,
    kind: InterpretErrorKind,
}

impl Error for InterpretError {}

impl Display for InterpretError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Interpreter {
    /// Creates a new interpreter with the given arguments
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
    ) -> Result<Interpreter, Box<dyn Error>> {
        let mut parser = Parser::new(file_name)?;
        let mut instructions = vec![];
        for instr in &mut parser {
            let instr = instr?;
            if ir {
                dbg!(&instr);
            }
            instructions.push(instr);
        }
        let stack = vec![];
        let call_stack = vec![];
        let heap = vec![0; heap_size];
        let mut labels = HashMap::new();
        let instruction_pointer = 0;
        let done = false;

        for (i, instr) in instructions.iter().enumerate() {
            if instr.cmd == CommandKind::Mark {
                if let Some(ParamKind::Label(label)) = instr.param.clone() {
                    labels.insert(label, i);
                }
            }
        }

        Ok(Interpreter {
            instructions,
            stack,
            call_stack,
            heap,
            labels,
            instruction_pointer,
            debug,
            debug_heap,
            suppress_output,
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
    pub fn run(&mut self) -> Result<(), Box<dyn Error>> {
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

    fn stack(&mut self, instr: Instruction) -> Result<(), Box<dyn Error>> {
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

    fn arithmetic(&mut self, instr: Instruction) -> Result<(), Box<dyn Error>> {
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

    fn heap(&mut self, instr: Instruction) -> Result<(), Box<dyn Error>> {
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

    fn flow(&mut self, instr: Instruction) -> Result<(), Box<dyn Error>> {
        match instr.cmd {
            CommandKind::Mark => Ok(()),
            CommandKind::Call => {
                if let Some(ParamKind::Label(label)) = &instr.param {
                    if let Some(index) = self.labels.get(label) {
                        self.call_stack.push(self.instruction_pointer);
                        self.instruction_pointer = *index;

                        return Ok(());
                    }

                    return InterpretErrorKind::UnknownLabel(instr).throw();
                }

                InterpretErrorKind::ParseLogicError(instr).throw()
            }
            CommandKind::Jump => {
                if let Some(ParamKind::Label(label)) = &instr.param {
                    if let Some(index) = self.labels.get(label) {
                        self.instruction_pointer = *index;

                        return Ok(());
                    }

                    return InterpretErrorKind::UnknownLabel(instr).throw();
                }

                InterpretErrorKind::ParseLogicError(instr).throw()
            }
            CommandKind::JumpZero => {
                if let Some(val) = self.stack.pop() {
                    if val != 0 {
                        return Ok(());
                    }
                    if let Some(ParamKind::Label(label)) = &instr.param {
                        if let Some(index) = self.labels.get(label) {
                            self.instruction_pointer = *index;

                            return Ok(());
                        }

                        return InterpretErrorKind::UnknownLabel(instr).throw();
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
                    if let Some(ParamKind::Label(label)) = &instr.param {
                        if let Some(index) = self.labels.get(label) {
                            self.instruction_pointer = *index;

                            return Ok(());
                        }

                        return InterpretErrorKind::UnknownLabel(instr).throw();
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

    fn io(&mut self, instr: Instruction) -> Result<(), Box<dyn Error>> {
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
                    if self.suppress_output {
                        return Ok(());
                    }
                    if let Some(character) = char::from_u32(character as u32) {
                        write!(stdout(), "{}", character)?;
                        stdout().flush()?;

                        return Ok(());
                    }
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::OutInteger => {
                if let Some(number) = self.stack.pop() {
                    if self.suppress_output {
                        return Ok(());
                    }
                    write!(stdout(), "{}", number)?;
                    stdout().flush()?;

                    return Ok(());
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::ReadCharacter => {
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

                    stdout().flush()?;
                    return match Getch::new().getch() {
                        Ok(val) => {
                            self.heap[addr as usize] = val as i32;
                            write!(stdout(), "{}", char::from_u32(val as u32).unwrap())?;
                            stdout().flush()?;
                            Ok(())
                        }
                        Err(err) => Err(Box::new(err)),
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
                    stdout().flush()?;
                    let mut input_text = String::new();
                    stdin().read_line(&mut input_text)?;

                    let trimmed = input_text.trim();
                    let num = trimmed.parse::<i32>()?;
                    self.heap[addr as usize] = num;

                    return Ok(());
                }

                InterpretErrorKind::StdinError(instr).throw()
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
    pub fn exec(&mut self, instr: Instruction) -> Result<(), Box<dyn Error>> {
        if self.debug {
            dbg!(&self.stack);
            dbg!(&self.call_stack);
            dbg!(&self.instruction_pointer);
            dbg!(&self.instructions[self.instruction_pointer]);
        }
        if self.debug_heap {
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
    use super::Interpreter;
    use std::error::Error;

    #[test]
    fn interpret_stack() -> Result<(), Box<dyn Error>> {
        let mut interpreter = Interpreter::new("ws/interpret_stack.ws", 0, true, true, true, true)?;

        interpreter.run()?;

        assert_eq!(interpreter.stack, vec![-1]);
        assert!(interpreter.heap.is_empty());
        assert!(interpreter.labels.is_empty());

        Ok(())
    }

    #[test]
    fn interpret_arithmetic() -> Result<(), Box<dyn Error>> {
        let mut interpreter =
            Interpreter::new("ws/interpret_arithmetic.ws", 0, true, true, true, true)?;

        interpreter.run()?;

        assert_eq!(interpreter.stack, vec![4]);
        assert!(interpreter.heap.is_empty());
        assert!(interpreter.labels.is_empty());

        Ok(())
    }

    #[test]
    fn interpret_heap() -> Result<(), Box<dyn Error>> {
        let mut interpreter = Interpreter::new("ws/interpret_heap.ws", 1, true, true, true, true)?;

        interpreter.run()?;

        assert_eq!(interpreter.stack, vec![-8, 10]);

        Ok(())
    }

    #[test]
    fn interpret_flow() -> Result<(), Box<dyn Error>> {
        let mut interpreter = Interpreter::new("ws/interpret_flow.ws", 0, true, true, true, true)?;

        interpreter.run()?;
        assert_eq!(interpreter.stack, vec![]);

        Ok(())
    }

    #[test]
    fn interpret_io() -> Result<(), Box<dyn Error>> {
        let mut interpreter = Interpreter::new("ws/interpret_io.ws", 0, true, true, true, true)?;

        interpreter.run()?;

        Ok(())
    }
}
