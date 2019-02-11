use disassembler::Reg;

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum ILOperand {
    Value(i32),
    Register(Reg),
    Argument(i32),
    Local(i32),
    Pointer(Reg, i32),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct BinaryInstruction<Op : Clone> {
    pub left: Op,
    pub right: Op,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct UnaryInstruction<Op> {
    pub operand: Op,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum BranchType {
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    GreaterOrEqual,
    Greater,
    Unconditional,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct BranchInstruction<Op : Clone> {
    pub type_: BranchType,
    pub condition: Option<BinaryInstruction<Op>>,
    pub target: u64,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
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

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum ILUnaryOperator {
    Negate,
    Not,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
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
