#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operator {
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,

    Not,
    And,
    Or,
    Xor,
    ShiftLeft,
    ShiftRight,

    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Var {
    Var(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Binary(Operator, Box<Expression>, Box<Expression>),
    Dereference(Box<Expression>),
    Unary(Operator, Box<Expression>),
    Value(i32),
    VarRef(Var),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Assignment(Var, Expression),
    Break,
    Continue,
    DoWhile(Vec<Statement>, Expression),
    For(Expression, Vec<Statement>, Vec<Statement>),
    FunctionCall(Expression),
    IfThenElse(Expression, Vec<Statement>, Vec<Statement>),
    Return(Option<Var>),
    While(Expression, Vec<Statement>),
}
