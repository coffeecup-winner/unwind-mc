use std::collections::HashMap;

use ast::*;
use transformer::*;
use type_resolver::*;

pub struct T<'a> {
    types: &'a HashMap<String, DataType>,
}

impl<'a> T<'a> {
    pub fn new(types: &'a HashMap<String, DataType>) -> T {
        T { types: types }
    }

    fn fixup(&self, op: Operator, var: Var, value: i32) -> Expression {
        let Var::Var(ref name) = var;
        let type_ = self.types.get(name).unwrap();
        use ast::Expression::*;
        use type_resolver::DataType::*;
        match type_ {
            Pointer(_) | Function => {
                if value % type_.size() as i32 != 0 {
                    panic!("Value size must be divisible by type size");
                }
                let new_value = Expression::Value(value / type_.size() as i32);
                Binary(op, Box::new(VarRef(var)), Box::new(new_value))
            }
            _ => Binary(op, Box::new(VarRef(var)), Box::new(Value(value))),
        }
    }
}

impl<'a> Transformer for T<'a> {
    fn transform_statement(&mut self, stmt: Statement) -> Statement {
        default_transform_statement(self, stmt)
    }

    fn transform_expression(&mut self, expr: Expression) -> Expression {
        use ast::Expression::*;
        use ast::Operator::*;
        match &expr {
            Binary(op, box VarRef(var), box Value(value))
            | Binary(op, box Value(value), box VarRef(var))
                if *op == Add || *op == Subtract =>
            {
                self.fixup(*op, var.clone(), value.clone())
            }
            _ => default_transform_expression(self, expr),
        }
    }
}
