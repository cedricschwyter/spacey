use std::rc::Rc;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Number {
    pub value: i32,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Label {
    pub value: Rc<str>,
    pub index: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Instruction {
    PushStack(Number),
    DuplicateStack,
    CopyNthStack(Number),
    SwapStack,
    DiscardStack,
    SlideNStack(Number),
    Add,
    Subtract,
    Multiply,
    IntegerDivision,
    Modulo,
    StoreHeap,
    RetrieveHeap,
    Mark(Label),
    Call(Label),
    Jump(Label),
    JumpZero(Label),
    JumpNegative(Label),
    Return,
    Exit,
    OutCharacter,
    OutInteger,
    ReadCharacter,
    ReadInteger,
}
