use libudis86_sys::ud_type;

pub type OperandType = ud_type;

#[derive(PartialEq, Eq, Hash)]
pub enum ILOperand {
    Value(i32),
    Register(OperandType),
    Argument(i32),
    Local(i32),
    Pointer(OperandType, i32),
}

#[derive(PartialEq, Eq, Hash)]
pub struct BinaryInstruction<Op> {
    pub left: Op,
    pub right: Op,
}

#[derive(PartialEq, Eq, Hash)]
pub struct UnaryInstruction<Op> {
    pub operand: Op,
}

#[derive(PartialEq, Eq, Hash)]
pub enum BranchType {
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    GreaterOrEqual,
    Greater,
    Unconditional,
}

#[derive(PartialEq, Eq, Hash)]
pub struct BranchInstruction {
    pub type_: BranchType,
    pub target: u64,
}

#[derive(PartialEq, Eq, Hash)]
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

#[derive(PartialEq, Eq, Hash)]
pub enum ILUnaryOperator {
    Negate,
    Not,
}

#[derive(PartialEq, Eq, Hash)]
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

pub fn unary<Op>(operand: Op) -> UnaryInstruction<Op> {
    UnaryInstruction { operand: operand }
}

pub fn binary<Op>(left: Op, right: Op) -> BinaryInstruction<Op> {
    BinaryInstruction {
        left: left,
        right: right,
    }
}

pub fn branch(type_: BranchType, target: u64) -> BranchInstruction {
    BranchInstruction {
        type_: type_,
        target: target,
    }
}
