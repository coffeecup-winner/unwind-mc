use ast::*;

pub trait Transformer {
    fn transform_statement(&mut self, stmt: Statement) -> Statement;
    fn transform_expression(&mut self, expr: Expression) -> Expression;
}

pub fn default_transform_statement(transformer: &mut Transformer, stmt: Statement) -> Statement {
    use ast::Statement::*;
    match stmt {
        Assignment(var, expr) => Assignment(var, transformer.transform_expression(expr)),
        Break => Break,
        Continue => Continue,
        DoWhile(body, condition) => DoWhile(
            body.into_iter()
                .map(|s| transformer.transform_statement(s))
                .collect(),
            transformer.transform_expression(condition),
        ),
        For(condition, modifier, body) => For(
            transformer.transform_expression(condition),
            modifier
                .into_iter()
                .map(|s| transformer.transform_statement(s))
                .collect(),
            body.into_iter()
                .map(|s| transformer.transform_statement(s))
                .collect(),
        ),
        FunctionCall(expr) => FunctionCall(transformer.transform_expression(expr)),
        IfThenElse(condition, true_branch, false_branch) => IfThenElse(
            transformer.transform_expression(condition),
            true_branch
                .into_iter()
                .map(|s| transformer.transform_statement(s))
                .collect(),
            false_branch
                .into_iter()
                .map(|s| transformer.transform_statement(s))
                .collect(),
        ),
        Return(var) => Return(var),
        While(condition, body) => While(
            transformer.transform_expression(condition),
            body.into_iter()
                .map(|s| transformer.transform_statement(s))
                .collect(),
        ),
    }
}

pub fn default_transform_expression(transformer: &mut Transformer, expr: Expression) -> Expression {
    use ast::Expression::*;
    match expr {
        Binary(op, left, right) => Binary(
            op,
            Box::new(transformer.transform_expression(*left)),
            Box::new(transformer.transform_expression(*right)),
        ),
        Dereference(expr) => Dereference(Box::new(transformer.transform_expression(*expr))),
        Unary(op, operand) => Unary(op, Box::new(transformer.transform_expression(*operand))),
        Value(value) => Value(value),
        VarRef(var) => VarRef(var),
    }
}
