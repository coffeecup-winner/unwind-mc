use disassembler::Reg;

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum ILOperand {
    Value(i32),
    Register(Reg),
    Argument(i32),
    Local(i32),
    Pointer(Reg, Reg, u8, u32),
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
pub struct CopyInstruction<Op> {
    pub dst: Op,
    pub src: Op,
    pub stride: Op,
    pub count: Op,
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
    LoadAddress,
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
    Copy(CopyInstruction<Op>),
    Branch(BranchInstruction<Op>),
    Call(UnaryInstruction<Op>),
    Return(UnaryInstruction<Op>), // TODO: remove data
    Continue,
    Break,
}

pub fn unary<Op>(operand: Op) -> UnaryInstruction<Op> {
    UnaryInstruction { operand }
}

pub fn binary<Op : Clone>(left: Op, right: Op) -> BinaryInstruction<Op> {
    BinaryInstruction { left, right }
}

pub fn copy<Op : Clone>(dst: Op, src: Op, stride: Op, count: Op) -> CopyInstruction<Op> {
    CopyInstruction { dst, src, stride, count }
}

pub fn branch<Op : Clone>(type_: BranchType, condition: Option<BinaryInstruction<Op>>, target: u64) -> BranchInstruction<Op> {
    BranchInstruction { type_, condition, target }
}

impl ILInstruction<ILOperand> {
    pub fn print_syntax(&self) -> String {
        let mut res = String::new();
        match self {
            ILInstruction::Binary(op, binary) => {
                res += Self::print_binary_operator(op);
                res += " ";
                res += &Self::print_operand(&binary.left);
                res += ", ";
                res += &Self::print_operand(&binary.right);
            },
            ILInstruction::Unary(op, unary) => {
                res += Self::print_unary_operator(op);
                res += " ";
                res += &Self::print_operand(&unary.operand);
            },
            ILInstruction::Assign(binary) => {
                res += &Self::print_operand(&binary.left);
                res += " := ";
                res += &Self::print_operand(&binary.right);
            },
            ILInstruction::Copy(copy) => {
                res += "copy (";
                res += &Self::print_operand(&copy.stride);
                res += " * ";
                res += &Self::print_operand(&copy.count);
                res += ") bytes from ";
                res += &Self::print_operand(&copy.src);
                res += " to ";
                res += &Self::print_operand(&copy.dst);
            }
            ILInstruction::Branch(branch) => {
                res += &Self::print_branch(branch);
            },
            ILInstruction::Call(unary) => {
                res += "call ";
                res += &Self::print_operand(&unary.operand);
            },
            ILInstruction::Return(unary) => {
                res += "ret ";
                res += &Self::print_operand(&unary.operand);
            },
            ILInstruction::Continue => {
                res += "continue";
            },
            ILInstruction::Break => {
                res += "break";
            },
        }
        res
    }

    fn print_binary_operator(op: &ILBinaryOperator) -> &'static str {
        use self::ILBinaryOperator::*;
        match op {
            Add => "add",
            And => "and",
            Divide => "div",
            Multiply => "mul",
            LoadAddress => "lea",
            Or => "or",
            ShiftLeft => "shl",
            ShiftRight => "shr",
            Subtract => "sub",
            Xor => "xor",
        }
    }

    fn print_unary_operator(op: &ILUnaryOperator) -> &'static str {
        use self::ILUnaryOperator::*;
        match op {
            Negate => "neg",
            Not => "not",
        }
    }

    fn print_branch_type(type_: &BranchType) -> &'static str {
        use self::BranchType::*;
        match type_ {
            Equal => "==",
            NotEqual => "!=",
            Less => "<",
            LessOrEqual => "<=",
            GreaterOrEqual => ">=",
            Greater => ">",
            Unconditional => panic!("Can't print unconditional branch type"),
        }
    }

    fn print_branch(br: &BranchInstruction<ILOperand>) -> String {
        let mut res = String::new();
        if br.type_ == BranchType::Unconditional {
            res += "jmp ";
            res += &br.target.to_string();
        } else {
            res += "br ";
            res += &br.target.to_string();
            res += " if ";
            if let Some(condition) = &br.condition {
                res += &Self::print_operand(&condition.left);
                res += " ";
                res += Self::print_branch_type(&br.type_);
                res += " ";
                res += &Self::print_operand(&condition.right);
            } else {
                panic!("Invalid branch instruction")
            }
        }
        res
    }

    fn print_operand(op: &ILOperand) -> String {
        let mut res = String::new();
        use self::ILOperand::*;
        match op {
            Value(v) => return v.to_string(),
            Register(r) => return String::from(r.to_str()),
            Argument(v) => {
                res += "arg(";
                res += &v.to_string();
                res += ")";
            }
            Local(v) => {
                res += "loc(";
                res += &v.to_string();
                res += ")";
            },
            &Pointer(b, i, s, o) => {
                res += "ptr(";
                if b != Reg::NONE {
                    res += b.to_str();
                    res += " + ";
                }
                if i != Reg::NONE && s != 0 {
                    res += i.to_str();
                    res += " * ";
                    res += &s.to_string();
                    res += " + ";
                }
                res += &o.to_string();
                res += ")";
            },
        }
        res
    }
}
