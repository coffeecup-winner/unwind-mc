use super::*;

pub type OperandType = TODO; // TODO: implement this

pub enum ILOperand {
    Value(i32),
    Register(OperandType),
    Argument(i32),
    Local(i32),
    Pointer(OperandType, i32),
}

pub struct BinaryInstruction<Op> {
    pub left: Op,
    pub right: Op,
}

pub struct UnaryInstruction<Op> {
    pub operand: Op,
}

pub enum BranchType {
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    GreaterOrEqual,
    Greater,
    Unconditional,
}

pub struct BranchInstruction {
    pub type_: BranchType,
    pub target: u64,
}

pub enum ILBinaryOperator {
    Add,
    And,
    Divide,
    Multiply,
    Or,
    ShiftLeft,
    ShiftRight,
    Subtract,
    Xor,
}

pub enum ILUnaryOperator {
    Negate,
    Not,
}

pub enum ILInstruction<Op> {
    Binary(ILBinaryOperator, BinaryInstruction<Op>),
    Unary(ILUnaryOperator, UnaryInstruction<Op>),
    Assign(BinaryInstruction<Op>),
    Branch(BranchInstruction),
    Call(UnaryInstruction<Op>),
    Compare(BinaryInstruction<Op>),
    Return(UnaryInstruction<Op>), // TODO: remove data
    Nop,                          // TODO: remove this
    Continue,
    Break,
}
