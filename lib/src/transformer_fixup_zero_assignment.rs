use ast::*;
use transformer::*;

#[derive(Default)]
pub struct T {}

impl Transformer for T {
    fn transform_statement(&mut self, stmt: Statement) -> Statement {
        use ast::Expression::*;
        use ast::Operator::*;
        use ast::Statement::*;
        match &stmt {
            Assignment(Var::Var(name), expr) => match expr {
                Binary(And, box VarRef(Var::Var(v)), box Value(0)) if name == v => {
                    Assignment(Var::Var(v.clone()), Value(0))
                }
                _ => default_transform_statement(self, stmt),
            },
            _ => default_transform_statement(self, stmt),
        }
    }

    fn transform_expression(&mut self, expr: Expression) -> Expression {
        default_transform_expression(self, expr)
    }
}
