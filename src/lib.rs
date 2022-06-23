#![feature(test)]

use memmap::Mmap;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::Display;
use std::fs::File;
use std::io;
use std::ops::Index;

const SPACE: u8 = b' ';
const TAB: u8 = b'\t';
const LINE_FEED: u8 = b'\n';

#[derive(Debug)]
struct Parser {
    source: Mmap,
    index: usize,
}

#[derive(Debug)]
struct StackFrame {
    caller_index: usize,
    stack: Vec<i32>,
}

impl StackFrame {
    fn new(caller_index: usize) -> StackFrame {
        StackFrame {
            caller_index,
            stack: vec![],
        }
    }

    fn len(&self) -> usize {
        self.stack.len()
    }

    fn push(&mut self, val: i32) {
        self.stack.push(val);
    }

    fn pop(&mut self) -> Option<i32> {
        self.stack.pop()
    }
}

impl Index<usize> for StackFrame {
    type Output = i32;

    fn index(&self, index: usize) -> &Self::Output {
        &self.stack[index]
    }
}

pub struct Interpreter {
    stack: Vec<StackFrame>,
    heap: Vec<i32>,
    labels: HashMap<String, usize>,
    instruction_pointer: usize,
    instructions: Vec<Instruction>,
}

#[derive(Debug)]
enum ParseErrorKind {
    InvalidToken(usize, Vec<u8>),
    UnexpectedToken(usize, u8, Vec<u8>),
}

impl ParseErrorKind {
    fn throw<T>(self) -> Result<T, Box<dyn Error>> {
        let msg = match &self {
            ParseErrorKind::UnexpectedToken(pos, token, tokens) => format!(
                "unexpected token at position {}, expected one of {:?}, but got {}",
                pos,
                tokens.iter().map(|b| *b as char).collect::<Vec<_>>(),
                *token as char
            ),
            ParseErrorKind::InvalidToken(pos, tokens) => format!(
                "unexpected token at position {}, expected one of {:?}",
                pos,
                tokens.iter().map(|b| *b as char).collect::<Vec<_>>(),
            ),
        };
        Err(Box::new(ParseError { msg, kind: self }))
    }
}

impl Display for ParseErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug)]
#[allow(dead_code)]
struct ParseError {
    msg: String,
    kind: ParseErrorKind,
}

impl Error for ParseError {}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug)]
enum InterpretErrorKind {
    ParseLogicError(Instruction),
    StackUnderflow(Instruction),
    NumberOutOfBoundsError(Instruction, i32, i32, i32),
    NoTermination(Instruction),
    UnknownLabel(Instruction),
    StdinError(Instruction, String),
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
            InterpretErrorKind::StdinError(instr, val) => format!("only one character allowed at a time, got {} when executing: {:?}", val, instr)
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

#[derive(Debug, PartialEq, Clone, Copy)]
enum ImpKind {
    Stack,
    Arithmetic,
    Heap,
    Flow,
    IO,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum CommandKind {
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
            | CommandKind::JumpNegative => Some(ParamKind::Label("".to_string())),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum ParamKind {
    Number(i32),
    Label(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Instruction {
    imp: ImpKind,
    cmd: CommandKind,
    param: Option<ParamKind>,
    start_index: usize,
}

impl Parser {
    fn new(file_name: &str) -> Result<Parser, Box<dyn Error>> {
        let file = File::open(&file_name)?;
        let source = unsafe { Mmap::map(&file)? };
        let index = 0;

        Ok(Parser { source, index })
    }

    fn next(&mut self) -> Option<u8> {
        let tokens = vec![SPACE, TAB, LINE_FEED];
        while self.index < self.source.len() - 1 {
            self.index += 1;
            let token = self.source[self.index - 1];
            if tokens.contains(&token) {
                return Some(token);
            }
        }
        None
    }

    fn imp(&mut self) -> Option<Result<ImpKind, Box<dyn Error>>> {
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
                                self.index,
                                val,
                                vec![SPACE, TAB, LINE_FEED],
                            )
                            .throw(),
                        ),
                    }
                } else {
                    Some(
                        ParseErrorKind::UnexpectedToken(
                            self.index,
                            val,
                            vec![SPACE, TAB, LINE_FEED],
                        )
                        .throw(),
                    )
                }
            }
            LINE_FEED => Some(Ok(ImpKind::Flow)),
            _ => Some(
                ParseErrorKind::UnexpectedToken(self.index, val, vec![SPACE, TAB, LINE_FEED])
                    .throw(),
            ),
        }
    }

    fn stack(&mut self) -> Option<Result<CommandKind, Box<dyn Error>>> {
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
                                self.index,
                                val,
                                vec![SPACE, LINE_FEED],
                            )
                            .throw(),
                        ),
                    };
                }
                Some(ParseErrorKind::InvalidToken(self.index, vec![SPACE, LINE_FEED]).throw())
            }
            LINE_FEED => {
                if let Some(val) = self.next() {
                    return match val {
                        SPACE => Some(Ok(CommandKind::DuplicateStack)),
                        TAB => Some(Ok(CommandKind::SwapStack)),
                        LINE_FEED => Some(Ok(CommandKind::DiscardStack)),
                        _ => Some(
                            ParseErrorKind::UnexpectedToken(
                                self.index,
                                val,
                                vec![SPACE, TAB, LINE_FEED],
                            )
                            .throw(),
                        ),
                    };
                }
                Some(ParseErrorKind::InvalidToken(self.index, vec![SPACE, TAB, LINE_FEED]).throw())
            }
            _ => Some(
                ParseErrorKind::UnexpectedToken(self.index, val, vec![SPACE, TAB, LINE_FEED])
                    .throw(),
            ),
        }
    }

    fn arithmetic(&mut self) -> Option<Result<CommandKind, Box<dyn Error>>> {
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
                                self.index,
                                val,
                                vec![SPACE, TAB, LINE_FEED],
                            )
                            .throw(),
                        ),
                    };
                }

                Some(ParseErrorKind::InvalidToken(self.index, vec![SPACE, TAB, LINE_FEED]).throw())
            }
            TAB => {
                if let Some(val) = self.next() {
                    return match val {
                        SPACE => Some(Ok(CommandKind::IntegerDivision)),
                        TAB => Some(Ok(CommandKind::Modulo)),
                        _ => {
                            Some(ParseErrorKind::InvalidToken(self.index, vec![SPACE, TAB]).throw())
                        }
                    };
                }
                Some(ParseErrorKind::InvalidToken(self.index, vec![SPACE, TAB]).throw())
            }
            _ => Some(ParseErrorKind::UnexpectedToken(self.index, val, vec![SPACE, TAB]).throw()),
        }
    }

    fn heap(&mut self) -> Option<Result<CommandKind, Box<dyn Error>>> {
        let val = self.next()?;
        match val {
            SPACE => Some(Ok(CommandKind::StoreHeap)),
            TAB => Some(Ok(CommandKind::RetrieveHeap)),
            _ => Some(ParseErrorKind::InvalidToken(self.index, vec![SPACE, TAB]).throw()),
        }
    }

    fn flow(&mut self) -> Option<Result<CommandKind, Box<dyn Error>>> {
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
                                self.index,
                                val,
                                vec![SPACE, TAB, LINE_FEED],
                            )
                            .throw(),
                        ),
                    };
                }

                Some(ParseErrorKind::InvalidToken(self.index, vec![SPACE, TAB, LINE_FEED]).throw())
            }
            TAB => {
                if let Some(val) = self.next() {
                    return match val {
                        SPACE => Some(Ok(CommandKind::JumpZero)),
                        TAB => Some(Ok(CommandKind::JumpNegative)),
                        LINE_FEED => Some(Ok(CommandKind::Return)),
                        _ => Some(
                            ParseErrorKind::UnexpectedToken(
                                self.index,
                                val,
                                vec![SPACE, TAB, LINE_FEED],
                            )
                            .throw(),
                        ),
                    };
                }

                Some(ParseErrorKind::InvalidToken(self.index, vec![SPACE, TAB, LINE_FEED]).throw())
            }
            LINE_FEED => {
                if let Some(val) = self.next() {
                    return match val {
                        LINE_FEED => Some(Ok(CommandKind::Exit)),
                        _ => Some(
                            ParseErrorKind::UnexpectedToken(self.index, val, vec![LINE_FEED])
                                .throw(),
                        ),
                    };
                }

                Some(ParseErrorKind::UnexpectedToken(self.index, val, vec![LINE_FEED]).throw())
            }
            _ => Some(
                ParseErrorKind::UnexpectedToken(self.index, val, vec![SPACE, TAB, LINE_FEED])
                    .throw(),
            ),
        }
    }

    fn io(&mut self) -> Option<Result<CommandKind, Box<dyn Error>>> {
        let val = self.next()?;
        match val {
            SPACE => {
                if let Some(val) = self.next() {
                    return match val {
                        SPACE => Some(Ok(CommandKind::OutCharacter)),
                        TAB => Some(Ok(CommandKind::OutInteger)),
                        _ => Some(
                            ParseErrorKind::UnexpectedToken(self.index, val, vec![SPACE, TAB])
                                .throw(),
                        ),
                    };
                }

                Some(ParseErrorKind::UnexpectedToken(self.index, val, vec![SPACE, TAB]).throw())
            }
            TAB => {
                if let Some(val) = self.next() {
                    return match val {
                        SPACE => Some(Ok(CommandKind::ReadCharacter)),
                        TAB => Some(Ok(CommandKind::ReadInteger)),
                        _ => Some(
                            ParseErrorKind::UnexpectedToken(self.index, val, vec![SPACE, TAB])
                                .throw(),
                        ),
                    };
                }

                Some(ParseErrorKind::UnexpectedToken(self.index, val, vec![SPACE, TAB]).throw())
            }
            _ => Some(ParseErrorKind::UnexpectedToken(self.index, val, vec![SPACE, TAB]).throw()),
        }
    }

    fn cmd(&mut self, imp: ImpKind) -> Option<Result<CommandKind, Box<dyn Error>>> {
        match imp {
            ImpKind::Stack => Some(self.stack()?),
            ImpKind::Arithmetic => Some(self.arithmetic()?),
            ImpKind::Heap => Some(self.heap()?),
            ImpKind::Flow => Some(self.flow()?),
            ImpKind::IO => Some(self.io()?),
        }
    }

    fn number(&mut self, sign: i32) -> Option<Result<ParamKind, Box<dyn Error>>> {
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
                        ParseErrorKind::UnexpectedToken(self.index, val, vec![SPACE, TAB]).throw(),
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

    fn label(&mut self) -> Option<Result<ParamKind, Box<dyn Error>>> {
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
                            self.index,
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

        Some(Ok(ParamKind::Label(result)))
    }

    fn param(&mut self, kind: ParamKind) -> Option<Result<ParamKind, Box<dyn Error>>> {
        match kind {
            ParamKind::Label(_) => self.label(),
            ParamKind::Number(_) => {
                if let Some(val) = self.next() {
                    return match val {
                        SPACE => self.number(1),
                        TAB => self.number(-1),
                        _ => Some(
                            ParseErrorKind::UnexpectedToken(self.index, val, vec![SPACE, TAB])
                                .throw(),
                        ),
                    };
                }
                Some(ParseErrorKind::InvalidToken(self.index, vec![SPACE, TAB]).throw())
            }
        }
    }

    fn instruction(&mut self) -> Option<Result<Instruction, Box<dyn Error>>> {
        let start_index = self.index;
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
                let instr = Instruction {
                    imp,
                    cmd,
                    param,
                    start_index,
                };

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
    type Item = Result<Instruction, Box<dyn Error>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.instruction()
    }
}

impl Interpreter {
    pub fn new(file_name: &str, heap_size: usize, ir: bool) -> Result<Interpreter, Box<dyn Error>> {
        let mut parser = Parser::new(file_name)?;
        let mut instructions = vec![];
        for instr in &mut parser {
            let instr = instr?;
            if ir {
                dbg!(&instr);
            }
            instructions.push(instr);
        }
        let stack = vec![StackFrame::new(0)];
        let heap = vec![0; heap_size];
        let mut labels = HashMap::new();
        let instruction_pointer = 0;

        for i in 0..instructions.len() {
            if instructions[i].cmd == CommandKind::Mark {
                if let Some(ParamKind::Label(label)) = instructions[i].param.clone() {
                    labels.insert(label, i);
                }
            }
        }

        Ok(Interpreter {
            instructions,
            stack,
            heap,
            labels,
            instruction_pointer,
        })
    }

    pub fn next_instruction(&mut self) -> Option<Instruction> {
        self.instruction_pointer += 1;
        if self.instruction_pointer < self.instructions.len() {
            return Some(self.instructions[self.instruction_pointer - 1].clone());
        }

        None
    }

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

    fn stack(&mut self, instr: Instruction) -> Result<(), Box<dyn Error>> {
        match instr.cmd {
            CommandKind::PushStack => {
                if let Some(ParamKind::Number(val)) = instr.param {
                    let length = self.stack.len();
                    self.stack[length - 1].push(val);

                    return Ok(());
                }

                InterpretErrorKind::ParseLogicError(instr).throw()
            }
            CommandKind::DuplicateStack => {
                let length = self.stack.len();
                if let Some(val) = self.stack[length - 1].pop() {
                    self.stack[length - 1].push(val);
                    self.stack[length - 1].push(val);

                    return Ok(());
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::CopyNthStack => {
                let length = self.stack.len();
                if let Some(ParamKind::Number(addr)) = instr.param {
                    if addr < 0 || addr as usize >= self.stack[length - 1].len() {
                        return InterpretErrorKind::NumberOutOfBoundsError(
                            instr,
                            addr,
                            0,
                            self.stack[length - 1].len() as i32 - 1,
                        )
                        .throw();
                    }
                    let addr = addr as usize;
                    let val = self.stack[length - 1][addr];
                    self.stack[length - 1].push(val);

                    return Ok(());
                }

                InterpretErrorKind::ParseLogicError(instr).throw()
            }
            CommandKind::SwapStack => {
                let length = self.stack.len();
                if let Some(val) = self.stack[length - 1].pop() {
                    if let Some(other) = self.stack[length - 1].pop() {
                        self.stack[length - 1].push(val);
                        self.stack[length - 1].push(other);

                        return Ok(());
                    }

                    return InterpretErrorKind::StackUnderflow(instr).throw();
                }
                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::DiscardStack => {
                let length = self.stack.len();
                if let Some(_) = self.stack[length - 1].pop() {
                    return Ok(());
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::SlideNStack => {
                let length = self.stack.len();
                if let Some(top) = self.stack[length - 1].pop() {
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
                            self.stack[length - 1].pop();
                        }
                        self.stack[length - 1].push(top);

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
                let length = self.stack.len();
                if let Some(left) = self.stack[length - 1].pop() {
                    if let Some(right) = self.stack[length - 1].pop() {
                        self.stack[length - 1].push(left + right);

                        return Ok(());
                    }
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::Subtract => {
                let length = self.stack.len();
                if let Some(left) = self.stack[length - 1].pop() {
                    if let Some(right) = self.stack[length - 1].pop() {
                        self.stack[length - 1].push(left - right);

                        return Ok(());
                    }
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::Multiply => {
                let length = self.stack.len();
                if let Some(left) = self.stack[length - 1].pop() {
                    if let Some(right) = self.stack[length - 1].pop() {
                        self.stack[length - 1].push(left * right);

                        return Ok(());
                    }
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::IntegerDivision => {
                let length = self.stack.len();
                if let Some(left) = self.stack[length - 1].pop() {
                    if let Some(right) = self.stack[length - 1].pop() {
                        self.stack[length - 1].push(left / right);

                        return Ok(());
                    }
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::Modulo => {
                let length = self.stack.len();
                if let Some(left) = self.stack[length - 1].pop() {
                    if let Some(right) = self.stack[length - 1].pop() {
                        self.stack[length - 1].push(left % right);

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
                let length = self.stack.len();
                if let Some(val) = self.stack[length - 1].pop() {
                    if let Some(addr) = self.stack[length - 1].pop() {
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
                let length = self.stack.len();
                if let Some(addr) = self.stack[length - 1].pop() {
                    if addr < 0 || addr as usize >= self.heap.len() {
                        return InterpretErrorKind::NumberOutOfBoundsError(
                            instr,
                            addr,
                            0,
                            self.heap.len() as i32 - 1,
                        )
                        .throw();
                    }

                    self.stack[length - 1].push(self.heap[addr as usize]);

                    return Ok(());
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            _ => InterpretErrorKind::ParseLogicError(instr).throw(),
        }
    }

    fn flow(&mut self, instr: Instruction) -> Result<(), Box<dyn Error>> {
        match instr.cmd {
            CommandKind::Mark => {
                if let Some(ParamKind::Label(label)) = instr.param {
                    self.labels.insert(label, instr.start_index);

                    return Ok(());
                }

                InterpretErrorKind::ParseLogicError(instr).throw()
            }
            CommandKind::Call => {
                if let Some(ParamKind::Label(label)) = &instr.param {
                    if let Some(index) = self.labels.get(label) {
                        self.instruction_pointer = *index;

                        self.stack.push(StackFrame::new(self.instruction_pointer));

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
                let length = self.stack.len();
                if let Some(val) = self.stack[length - 1].pop() {
                    if val != 0 {
                        return Ok(());
                    }
                }
                if let Some(ParamKind::Label(label)) = &instr.param {
                    if let Some(index) = self.labels.get(label) {
                        self.instruction_pointer = *index;

                        return Ok(());
                    }

                    return InterpretErrorKind::UnknownLabel(instr).throw();
                }

                InterpretErrorKind::ParseLogicError(instr).throw()
            }
            CommandKind::JumpNegative => {
                let length = self.stack.len();
                if let Some(val) = self.stack[length - 1].pop() {
                    if val >= 0 {
                        return Ok(());
                    }
                }
                if let Some(ParamKind::Label(label)) = &instr.param {
                    if let Some(index) = self.labels.get(label) {
                        self.instruction_pointer = *index;

                        return Ok(());
                    }

                    return InterpretErrorKind::UnknownLabel(instr).throw();
                }

                InterpretErrorKind::ParseLogicError(instr).throw()
            }
            CommandKind::Return => {
                if let Some(frame) = self.stack.pop() {
                    self.instruction_pointer = frame.caller_index + 1;
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::Exit => Ok(()),
            _ => InterpretErrorKind::ParseLogicError(instr).throw(),
        }
    }

    fn io(&mut self, instr: Instruction) -> Result<(), Box<dyn Error>> {
        match instr.cmd {
            CommandKind::OutCharacter => {
                let length = self.stack.len();
                if let Some(character) = self.stack[length - 1].pop() {
                    if character < 0 {
                        return InterpretErrorKind::NumberOutOfBoundsError(
                            instr,
                            character,
                            0,
                            i32::MAX,
                        )
                        .throw();
                    }
                    if let Some(character) = char::from_u32(character as u32) {
                        print!("{}", character);

                        return Ok(());
                    }
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::OutInteger => {
                let length = self.stack.len();
                if let Some(number) = self.stack[length - 1].pop() {
                    print!("{}", number);

                    return Ok(());
                }

                InterpretErrorKind::StackUnderflow(instr).throw()
            }
            CommandKind::ReadCharacter => {
                let mut input_text = String::new();
                io::stdin().read_line(&mut input_text)?;
                if input_text.len() != 2 {
                    return InterpretErrorKind::StdinError(instr, input_text).throw();
                }

                let length = self.stack.len();

                let input_text = input_text.trim();
                if let Some(input) = input_text.chars().next() {
                    self.stack[length - 1].push(input as i32);

                    return Ok(());
                }

                InterpretErrorKind::StdinError(instr, input_text.to_string()).throw()
            }
            CommandKind::ReadInteger => {
                let mut input_text = String::new();
                io::stdin().read_line(&mut input_text)?;

                let length = self.stack.len();

                let trimmed = input_text.trim();
                let num = trimmed.parse::<i32>()?;
                self.stack[length - 1].push(num);

                Ok(())
            }
            _ => InterpretErrorKind::ParseLogicError(instr).throw(),
        }
    }

    pub fn exec(&mut self, instr: Instruction) -> Result<(), Box<dyn Error>> {
        match instr.imp {
            ImpKind::Stack => self.stack(instr),
            ImpKind::Arithmetic => self.arithmetic(instr),
            ImpKind::Heap => self.heap(instr),
            ImpKind::Flow => self.flow(instr),
            ImpKind::IO => self.io(instr),
        }
    }
}

#[cfg(test)]
mod tests {
    use clap::Result;

    use crate::ParamKind;

    use super::{CommandKind, ImpKind, Instruction, Interpreter};
    use std::error::Error;

    extern crate test;

    use test::Bencher;

    fn test_parse(
        interpreter: &mut Interpreter,
        results: Vec<Instruction>,
    ) -> Result<(), Box<dyn Error>> {
        let mut i = 0;
        while let Some(instr) = interpreter.next_instruction() {
            if i == results.len() {
                break;
            }
            assert_eq!(instr, results[i]);
            i += 1;
        }

        Ok(())
    }

    #[test]
    fn parse_stack() -> Result<(), Box<dyn Error>> {
        let mut interpreter = Interpreter::new("ws/parse_stack.ws", 0, true)?;
        let results = vec![
            Instruction {
                imp: ImpKind::Stack,
                cmd: CommandKind::PushStack,
                param: Some(ParamKind::Number(64)),
                start_index: 0,
            },
            Instruction {
                imp: ImpKind::Stack,
                cmd: CommandKind::DuplicateStack,
                param: None,
                start_index: 11,
            },
            Instruction {
                imp: ImpKind::Stack,
                cmd: CommandKind::CopyNthStack,
                param: Some(ParamKind::Number(64)),
                start_index: 14,
            },
            Instruction {
                imp: ImpKind::Stack,
                cmd: CommandKind::SwapStack,
                param: None,
                start_index: 26,
            },
            Instruction {
                imp: ImpKind::Stack,
                cmd: CommandKind::DiscardStack,
                param: None,
                start_index: 29,
            },
            Instruction {
                imp: ImpKind::Stack,
                cmd: CommandKind::SlideNStack,
                param: Some(ParamKind::Number(64)),
                start_index: 32,
            },
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::Exit,
                param: None,
                start_index: 44,
            },
        ];

        test_parse(&mut interpreter, results)
    }

    #[test]
    fn parse_arithmetic() -> Result<(), Box<dyn Error>> {
        let mut interpreter = Interpreter::new("ws/parse_arithmetic.ws", 0, true)?;
        let results = vec![
            Instruction {
                imp: ImpKind::Arithmetic,
                cmd: CommandKind::Add,
                param: None,
                start_index: 0,
            },
            Instruction {
                imp: ImpKind::Arithmetic,
                cmd: CommandKind::Subtract,
                param: None,
                start_index: 4,
            },
            Instruction {
                imp: ImpKind::Arithmetic,
                cmd: CommandKind::Multiply,
                param: None,
                start_index: 8,
            },
            Instruction {
                imp: ImpKind::Arithmetic,
                cmd: CommandKind::IntegerDivision,
                param: None,
                start_index: 12,
            },
            Instruction {
                imp: ImpKind::Arithmetic,
                cmd: CommandKind::Modulo,
                param: None,
                start_index: 16,
            },
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::Exit,
                param: None,
                start_index: 20,
            },
        ];

        test_parse(&mut interpreter, results)
    }

    #[test]
    fn parse_heap() -> Result<(), Box<dyn Error>> {
        let mut interpreter = Interpreter::new("ws/parse_heap.ws", 0, true)?;
        let results = vec![
            Instruction {
                imp: ImpKind::Heap,
                cmd: CommandKind::StoreHeap,
                param: None,
                start_index: 0,
            },
            Instruction {
                imp: ImpKind::Heap,
                cmd: CommandKind::RetrieveHeap,
                param: None,
                start_index: 3,
            },
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::Exit,
                param: None,
                start_index: 6,
            },
        ];

        test_parse(&mut interpreter, results)
    }

    #[test]
    fn parse_flow() -> Result<(), Box<dyn Error>> {
        let mut interpreter = Interpreter::new("ws/parse_flow.ws", 0, true)?;
        let results = vec![
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::Mark,
                param: Some(ParamKind::Label(" \t \t \t".to_string())),
                start_index: 0,
            },
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::Call,
                param: Some(ParamKind::Label(" \t \t \t".to_string())),
                start_index: 10,
            },
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::Jump,
                param: Some(ParamKind::Label(" \t \t \t".to_string())),
                start_index: 20,
            },
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::JumpZero,
                param: Some(ParamKind::Label(" \t \t \t".to_string())),
                start_index: 30,
            },
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::JumpNegative,
                param: Some(ParamKind::Label(" \t \t \t".to_string())),
                start_index: 40,
            },
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::Return,
                param: None,
                start_index: 50,
            },
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::Exit,
                param: None,
                start_index: 53,
            },
        ];

        test_parse(&mut interpreter, results)
    }

    #[test]
    fn parse_io() -> Result<(), Box<dyn Error>> {
        let mut interpreter = Interpreter::new("ws/parse_io.ws", 0, true)?;
        let results = vec![
            Instruction {
                imp: ImpKind::IO,
                cmd: CommandKind::OutCharacter,
                param: None,
                start_index: 0,
            },
            Instruction {
                imp: ImpKind::IO,
                cmd: CommandKind::OutInteger,
                param: None,
                start_index: 4,
            },
            Instruction {
                imp: ImpKind::IO,
                cmd: CommandKind::ReadCharacter,
                param: None,
                start_index: 8,
            },
            Instruction {
                imp: ImpKind::IO,
                cmd: CommandKind::ReadInteger,
                param: None,
                start_index: 12,
            },
            Instruction {
                imp: ImpKind::Flow,
                cmd: CommandKind::Exit,
                param: None,
                start_index: 16,
            },
        ];

        test_parse(&mut interpreter, results)
    }

    #[test]
    fn interpret_stack() -> Result<(), Box<dyn Error>> {
        let mut interpreter = Interpreter::new("ws/interpret_stack.ws", 0, true)?;

        interpreter.run()?;

        assert_eq!(interpreter.stack[0].stack, vec![-1]);
        assert!(interpreter.heap.is_empty());
        assert!(interpreter.labels.is_empty());

        Ok(())
    }

    #[test]
    fn interpret_arithmetic() -> Result<(), Box<dyn Error>> {
        let mut interpreter = Interpreter::new("ws/interpret_arithmetic.ws", 0, true)?;

        interpreter.run()?;

        assert_eq!(interpreter.stack[0].stack, vec![3]);
        assert!(interpreter.heap.is_empty());
        assert!(interpreter.labels.is_empty());

        Ok(())
    }

    #[test]
    fn interpret_heap() -> Result<(), Box<dyn Error>> {
        let mut interpreter = Interpreter::new("ws/interpret_heap.ws", 1, true)?;

        interpreter.run()?;

        assert_eq!(interpreter.stack[0].stack, vec![-8, 10]);

        Ok(())
    }

    #[test]
    fn interpret_io() -> Result<(), Box<dyn Error>> {
        let mut interpreter = Interpreter::new("ws/hello_world.ws", 0, true)?;

        interpreter.run()?;

        Ok(())
    }

    #[bench]
    fn bench_interpret(b: &mut Bencher) {
        b.iter(|| -> Result<(), Box<dyn Error>> {
            let mut interpreter = Interpreter::new("ws/hello_world.ws", 0, true)?;
            interpreter.run()?;
            Ok(())
        });
    }
}
