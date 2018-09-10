use std::collections::{HashMap, HashSet};

use ast::*;
use ast_builder::*;
use text_writer::*;
use type_resolver::*;

pub struct CppEmitter {
    parameter_names: HashSet<String>,
    types: HashMap<String, DataType>,
    text_writer: TextWriter,
}

impl CppEmitter {
    pub fn emit(func: &Function) -> String {
        let mut emitter = CppEmitter {
            parameter_names: HashSet::new(),
            types: HashMap::new(),
            text_writer: TextWriter::new(2),
        };
        for param in func.parameters.iter() {
            emitter.parameter_names.insert(param.name.clone());
            emitter
                .types
                .insert(param.name.clone(), param.type_.clone());
        }
        for var in func.locals.iter().chain(func.variables.iter()) {
            emitter.types.insert(var.name.clone(), var.type_.clone());
        }
        emitter.emit_signature(func, CppEmitter::find_return_var(&func.body));
        emitter.emit_body(func);
        emitter.text_writer.build_text()
    }

    fn find_return_var(stmts: &Vec<Statement>) -> &Option<Var> {
        // TODO: rewrite Function to include return var
        match stmts.iter().rev().nth(0).unwrap() {
            Statement::Return(var) => var,
            _ => &None,
        }
    }

    fn emit_signature(&mut self, func: &Function, ret: &Option<Var>) {
        self.emit_return_type(ret);
        self.text_writer.write(&func.name);
        self.text_writer.write("(");
        for i in 0..func.parameters.len() {
            if i != 0 {
                self.text_writer.write(",");
                self.text_writer.write(" ");
            }
            self.emit_declaration(&func.parameters[i].name, &func.parameters[i].type_);
        }
        self.text_writer.write(")");
        self.text_writer.write_newline();
    }

    fn emit_return_type(&mut self, var: &Option<Var>) {
        match var {
            None => self.text_writer.write("void"),
            Some(Var::Var(name)) => {
                let type_ = self.types[name].clone();
                match type_ {
                    DataType::Function => panic!("FIXME: emitting functions not supported yet"),
                    _ => (),
                };
                self.text_writer.write("int");
                self.text_writer.write(" ");
                self.emit_pointer_stars(&type_);
            }
        }
    }

    fn emit_body(&mut self, func: &Function) {
        self.text_writer.write("{");
        self.text_writer.write_newline();
        self.text_writer.increase_indent();

        self.emit_declarations(&func.locals, &func.variables);
        for stmt in &func.body {
            self.emit_statement(stmt);
        }

        self.text_writer.decrease_indent();
        self.text_writer.write("}");
        self.text_writer.write_newline();
    }

    fn emit_declarations(&mut self, locals: &Vec<Variable>, variables: &Vec<Variable>) {
        for var in locals.iter().chain(variables.iter()) {
            self.emit_declaration(&var.name, &var.type_);
            self.text_writer.write(";");
            self.text_writer.write_newline();
        }
        self.text_writer.write_newline();
    }

    fn emit_declaration(&mut self, name: &String, type_: &DataType) {
        if type_.is_function() {
            self.text_writer.write("void");
            self.text_writer.write(" ");
            self.text_writer.write("(");
            self.text_writer.write("*");
            self.emit_pointer_stars(type_);
            self.text_writer.write(name);
            self.text_writer.write(")");
            self.text_writer.write("()");
        } else {
            self.text_writer.write("int");
            self.text_writer.write(" ");
            self.emit_pointer_stars(type_);
            self.text_writer.write(name);
        }
    }

    fn emit_pointer_stars(&mut self, type_: &DataType) {
        match type_ {
            DataType::Pointer(t) => {
                self.text_writer.write("*");
                self.emit_pointer_stars(t);
            }
            _ => (),
        }
    }

    fn emit_statement(&mut self, stmt: &Statement) {
        use ast::Statement::*;
        match stmt {
            Assignment(var, expr) => self.emit_assignment(var, expr, true),
            Break => self.emit_break(),
            Continue => self.emit_continue(),
            DoWhile(body, cond) => self.emit_do_while(body, cond),
            For(cond, modifier, body) => self.emit_for(cond, modifier, body),
            FunctionCall(expr) => self.emit_function_call(expr),
            IfThenElse(cond, true_branch, false_branch) => {
                self.emit_if_then_else(cond, true_branch, false_branch)
            }
            Return(var) => self.emit_return(var),
            While(cond, body) => self.emit_while(cond, body),
        }
    }

    fn emit_scope(&mut self, stmts: &Vec<Statement>, skip_newline: bool) {
        self.text_writer.write("{");
        self.text_writer.write_newline();

        self.text_writer.increase_indent();
        for stmt in stmts.iter() {
            self.emit_statement(stmt);
        }
        self.text_writer.decrease_indent();

        self.text_writer.write("}");
        if !skip_newline {
            self.text_writer.write_newline();
        }
    }

    fn emit_assignment(&mut self, var: &Var, expr: &Expression, emit_separator: bool) {
        let Var::Var(name) = var;
        self.text_writer.write(name);
        self.text_writer.write(" ");
        self.text_writer.write("=");
        self.text_writer.write(" ");
        self.emit_expression(expr);
        if emit_separator {
            self.text_writer.write(";");
            self.text_writer.write_newline();
        }
    }

    fn emit_break(&mut self) {
        self.text_writer.write("break");
        self.text_writer.write(";");
        self.text_writer.write_newline();
    }

    fn emit_continue(&mut self) {
        self.text_writer.write("continue");
        self.text_writer.write(";");
        self.text_writer.write_newline();
    }

    fn emit_do_while(&mut self, body: &Vec<Statement>, cond: &Expression) {
        self.text_writer.write("do");
        self.text_writer.write_newline();

        self.emit_scope(body, true);
        self.text_writer.write(" ");
        self.text_writer.write("while");
        self.text_writer.write(" ");
        self.text_writer.write("(");
        self.emit_expression(cond);
        self.text_writer.write(")");
        self.text_writer.write(";");
        self.text_writer.write_newline();
    }

    fn emit_for(&mut self, cond: &Expression, modifier: &Vec<Statement>, body: &Vec<Statement>) {
        self.text_writer.write("for");
        self.text_writer.write(" ");
        self.text_writer.write("(");
        self.text_writer.write(";");
        self.text_writer.write(" ");
        self.emit_expression(cond);
        self.text_writer.write(";");
        self.text_writer.write(" ");
        for (i, stmt) in modifier.iter().enumerate() {
            match stmt {
                Statement::Assignment(var, expr) => self.emit_assignment(var, expr, false),
                _ => panic!("Impossible"),
            }
            if i < modifier.len() - 1 {
                self.text_writer.write(",");
                self.text_writer.write(" ");
            }
        }
        self.text_writer.write(")");
        self.text_writer.write_newline();

        self.emit_scope(body, false);
    }

    fn emit_function_call(&mut self, expr: &Expression) {
        self.emit_expression(expr);
        self.text_writer.write("()");
        self.text_writer.write(";");
        self.text_writer.write_newline();
    }

    fn emit_if_then_else(
        &mut self,
        cond: &Expression,
        true_branch: &Vec<Statement>,
        false_branch: &Vec<Statement>,
    ) {
        self.text_writer.write("if");
        self.text_writer.write(" ");
        self.text_writer.write("(");
        self.emit_expression(cond);
        self.text_writer.write(")");
        self.text_writer.write_newline();

        self.emit_scope(true_branch, false);
        if false_branch.len() > 0 {
            self.text_writer.write("else");
            self.text_writer.write_newline();
            self.emit_scope(false_branch, false);
        }
    }

    fn emit_return(&mut self, var: &Option<Var>) {
        self.text_writer.write("return");
        if let Some(Var::Var(name)) = var {
            self.text_writer.write(" ");
            self.text_writer.write(name);
        }
        self.text_writer.write(";");
        self.text_writer.write_newline();
    }

    fn emit_while(&mut self, condition: &Expression, body: &Vec<Statement>) {
        self.text_writer.write("while");
        self.text_writer.write(" ");
        self.text_writer.write("(");
        self.emit_expression(condition);
        self.text_writer.write(")");
        self.text_writer.write_newline();

        self.emit_scope(body, false);
    }

    fn emit_expression(&mut self, expr: &Expression) {
        use ast::Expression::*;
        match expr {
            Binary(op, left, right) => self.emit_binary(op, left, right),
            Dereference(expr) => self.emit_dereference(expr),
            Unary(op, operand) => self.emit_unary(op, operand),
            Value(value) => self.emit_value(value),
            VarRef(var) => self.emit_var(var),
        }
    }

    fn emit_binary(&mut self, op: &Operator, left: &Expression, right: &Expression) {
        self.emit_expression(left);
        self.text_writer.write(" ");
        use ast::Operator::*;
        self.text_writer.write(match op {
            Equal => "==",
            NotEqual => "!=",
            Less => "<",
            LessOrEqual => "<=",
            Greater => ">",
            GreaterOrEqual => ">=",
            And => "&",
            Or => "|",
            Xor => "^",
            ShiftLeft => "<<",
            ShiftRight => ">>",
            Add => "+",
            Subtract => "-",
            Multiply => "*",
            Divide => "/",
            Modulo => "%",
            _ => panic!("Not supported"),
        });
        self.text_writer.write(" ");
        self.emit_expression(right);
    }

    fn emit_dereference(&mut self, expr: &Expression) {
        self.text_writer.write("*");
        self.text_writer.write("(");
        self.emit_expression(expr);
        self.text_writer.write(")");
    }

    fn emit_unary(&mut self, op: &Operator, operand: &Expression) {
        self.text_writer.write(match op {
            Operator::Negate => "-",
            Operator::Not => "~",
            _ => panic!("Not supported"),
        });
        self.emit_expression(operand);
    }

    fn emit_value(&mut self, value: &i32) {
        self.text_writer.write(&value.to_string());
    }

    fn emit_var(&mut self, var: &Var) {
        let Var::Var(name) = var;
        self.text_writer.write(&name);
    }
}
