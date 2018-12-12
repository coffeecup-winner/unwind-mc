use libudis86_sys::ud_type;

pub type OperandType = ud_type;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum ILOperand {
    Value(i32),
    Register(OperandType),
    Argument(i32),
    Local(i32),
    Pointer(OperandType, i32),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct BinaryInstruction<Op : Clone> {
    pub left: Op,
    pub right: Op,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct UnaryInstruction<Op> {
    pub operand: Op,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum BranchType {
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    GreaterOrEqual,
    Greater,
    Unconditional,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct BranchInstruction<Op : Clone> {
    pub type_: BranchType,
    pub condition: Option<BinaryInstruction<Op>>,
    pub target: u64,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
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

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum ILUnaryOperator {
    Negate,
    Not,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum ILInstruction<Op : Clone> {
    Binary(ILBinaryOperator, BinaryInstruction<Op>),
    Unary(ILUnaryOperator, UnaryInstruction<Op>),
    Assign(BinaryInstruction<Op>),
    Branch(BranchInstruction<Op>),
    Call(UnaryInstruction<Op>),
    Return(UnaryInstruction<Op>), // TODO: remove data
    Nop,                          // TODO: remove this
    Continue,
    Break,
}

pub fn unary<Op>(operand: Op) -> UnaryInstruction<Op> {
    UnaryInstruction { operand }
}

pub fn binary<Op : Clone>(left: Op, right: Op) -> BinaryInstruction<Op> {
    BinaryInstruction { left, right }
}

pub fn branch<Op : Clone>(type_: BranchType, condition: Option<BinaryInstruction<Op>>, target: u64) -> BranchInstruction<Op> {
    BranchInstruction { type_, condition, target }
}
