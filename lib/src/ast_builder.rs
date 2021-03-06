use std::collections::{BTreeMap, HashMap};

use ast::*;
use common::REGISTER_SIZE;
use flow_analyzer::*;
use il::*;
use transformer::*;
use transformer_fixup_pointer_arithmetics;
use transformer_fixup_zero_assignment;
use type_resolver::*;

type ResolvedBlock = Block<ResolvedOperand>;
type Instruction = ILInstruction<ResolvedOperand>;

pub struct AstBuilder<'a> {
    variable_types: &'a Vec<DataType>,
    variable_names: BTreeMap<i32, String>,
    parameter_names: BTreeMap<i32, String>,
    local_names: BTreeMap<i32, String>,
    types: HashMap<String, DataType>,
    next_var_name_idx: i32,
}

#[derive(Debug)]
pub struct Variable {
    pub name: String,
    pub type_: DataType,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<Variable>,
    pub locals: Vec<Variable>,
    pub variables: Vec<Variable>,
    pub body: Vec<Statement>,
}

impl<'a> AstBuilder<'a> {
    pub fn build_ast(name: String, blocks: &[ResolvedBlock], types: &Result) -> Function {
        let mut builder = AstBuilder {
            variable_types: &types.variable_types,
            variable_names: BTreeMap::new(),
            parameter_names: BTreeMap::new(),
            local_names: BTreeMap::new(),
            types: HashMap::new(),
            next_var_name_idx: 0,
        };

        let mut offset = 0;
        for index in 0..types.parameter_types.len() {
            let name = String::from("arg") + &index.to_string();
            builder.parameter_names.insert(offset, name.clone());
            builder
                .types
                .insert(name.clone(), types.parameter_types[index].clone());
            offset += types.parameter_types[index].size() as i32;
        }
        offset -= REGISTER_SIZE as i32 * 2; // TODO: this will not always be correct
        for index in 0..types.local_types.len() {
            offset -= types.local_types[index].size() as i32;
            let name = String::from("loc") + &index.to_string();
            builder.local_names.insert(offset, name.clone());
            builder
                .types
                .insert(name.clone(), types.local_types[index].clone());
        }

        let mut body = builder.build_scope(blocks);

        let transformers: Vec<Box<dyn Transformer>> = vec![
            Box::new(transformer_fixup_pointer_arithmetics::T::new(
                &builder.types,
            )),
            Box::new(transformer_fixup_zero_assignment::T::default()),
        ];

        body = transformers.into_iter().fold(body, |b, mut t| {
            b.into_iter().map(|s| t.transform_statement(s)).collect()
        });

        Function {
            name,
            parameters: builder
                .parameter_names
                .values()
                .map(|v| Variable {
                    name: v.clone(),
                    type_: builder.types[v].clone(),
                })
                .collect(),
            locals: builder
                .local_names
                .values()
                .map(|v| Variable {
                    name: v.clone(),
                    type_: builder.types[v].clone(),
                })
                .collect(),
            variables: builder
                .variable_names
                .values()
                .map(|v| Variable {
                    name: v.clone(),
                    type_: builder.types[v].clone(),
                })
                .collect(),
            body,
        }
    }

    fn build_scope(&mut self, blocks: &[ResolvedBlock]) -> Vec<Statement> {
        let mut statements = vec![];
        for block in blocks.iter() {
            match block {
                Block::SequentialBlock(block) => {
                    for insn in block.instructions.iter() {
                        match insn {
                            insn => statements.push(self.build_statement(insn)),
                        }
                    }
                }
                Block::WhileBlock(block) => {
                    statements.push(self.build_while(&block.condition, &block.body));
                }
                Block::DoWhileBlock(block) => {
                    statements.push(self.build_do_while(&block.condition, &block.body));
                }
                Block::ForBlock(block) => {
                    statements.push(self.build_for(&block.condition, &block.modifier, &block.body));
                }
                Block::ConditionalBlock(block) => {
                    statements.push(self.build_if_then_else(
                        &block.condition,
                        &block.true_branch,
                        &block.false_branch,
                    ));
                }
            }
        }
        statements
    }

    fn build_while(&mut self, condition: &[Instruction], body: &[ResolvedBlock]) -> Statement {
        Statement::While(self.build_condition(condition), self.build_scope(body))
    }

    fn build_do_while(&mut self, condition: &[Instruction], body: &[ResolvedBlock]) -> Statement {
        Statement::DoWhile(self.build_scope(body), self.build_condition(condition))
    }

    fn build_for(
        &mut self,
        condition: &[Instruction],
        modifier: &[Instruction],
        body: &[ResolvedBlock],
    ) -> Statement {
        Statement::For(
            self.build_condition(condition),
            self.build_scope(&[Block::SequentialBlock(SequentialBlock {
                instructions: modifier.to_vec(),
            })]),
            self.build_scope(body),
        )
    }

    fn build_if_then_else(
        &mut self,
        condition: &[Instruction],
        true_branch: &[ResolvedBlock],
        false_branch: &[ResolvedBlock],
    ) -> Statement {
        Statement::IfThenElse(
            self.build_condition(condition),
            self.build_scope(true_branch),
            self.build_scope(false_branch),
        )
    }

    fn build_condition(&mut self, condition: &[Instruction]) -> Expression {
        match &condition[..] {
            [ILInstruction::Branch(branch)] => self.build_binary_operator(
                AstBuilder::get_binary_operator(&branch.type_),
                &branch.condition.clone().unwrap(),
            ),
            [ILInstruction::Assign(assign), ILInstruction::Branch(branch)] => {
                // TODO: this is a special case where we can inline the assigned variable, remove this
                let mut compare = branch.condition.clone().unwrap();
                compare.left = assign.right;
                self.build_binary_operator(AstBuilder::get_binary_operator(&branch.type_), &compare)
            }
            _ => panic!("Instruction is not a valid statement"),
        }
    }

    fn build_statement(&mut self, insn: &Instruction) -> Statement {
        match insn {
            ILInstruction::Binary(op, binary) => Statement::Assignment(
                self.build_var(&binary.left),
                self.build_binary_operator(AstBuilder::convert_binary_operator(op), binary),
            ),
            ILInstruction::Unary(op, unary) => Statement::Assignment(
                self.build_var(&unary.operand),
                self.build_unary_operator(AstBuilder::convert_unary_operator(op), unary),
            ),
            ILInstruction::Assign(binary) => Statement::Assignment(
                self.build_var(&binary.left),
                self.build_expression(&binary.right),
            ),
            ILInstruction::Copy(_copy) => panic!("Not supported yet"),
            ILInstruction::Call(unary) => {
                Statement::FunctionCall(self.build_expression(&unary.operand))
            }
            ILInstruction::Return(unary) => {
                let (_, id) = unary.operand;
                Statement::Return(id.map(|_| self.build_var(&unary.operand)))
            }
            ILInstruction::Continue => Statement::Continue,
            ILInstruction::Break => Statement::Break,
            ILInstruction::Branch(_) => panic!("impossble"),
        }
    }

    fn get_binary_operator(condition: &BranchType) -> Operator {
        match condition {
            BranchType::Equal => Operator::Equal,
            BranchType::NotEqual => Operator::NotEqual,
            BranchType::Less => Operator::Less,
            BranchType::LessOrEqual => Operator::LessOrEqual,
            BranchType::Greater => Operator::Greater,
            BranchType::GreaterOrEqual => Operator::GreaterOrEqual,
            BranchType::Unconditional => panic!("impossible"),
        }
    }

    fn convert_binary_operator(op: &ILBinaryOperator) -> Operator {
        match op {
            ILBinaryOperator::Add => Operator::Add,
            ILBinaryOperator::And => Operator::And,
            ILBinaryOperator::Divide => Operator::Divide,
            ILBinaryOperator::LoadAddress => Operator::LoadAddress,
            ILBinaryOperator::Multiply => Operator::Multiply,
            ILBinaryOperator::Or => Operator::Or,
            ILBinaryOperator::ShiftLeft => Operator::ShiftLeft,
            ILBinaryOperator::ShiftRight => Operator::ShiftRight,
            ILBinaryOperator::Subtract => Operator::Subtract,
            ILBinaryOperator::Xor => Operator::Xor,
        }
    }

    fn convert_unary_operator(op: &ILUnaryOperator) -> Operator {
        match op {
            ILUnaryOperator::Negate => Operator::Negate,
            ILUnaryOperator::Not => Operator::Not,
        }
    }

    fn build_binary_operator(
        &mut self,
        op: Operator,
        insn: &BinaryInstruction<ResolvedOperand>,
    ) -> Expression {
        Expression::Binary(
            op,
            Box::new(self.build_expression(&insn.left)),
            Box::new(self.build_expression(&insn.right)),
        )
    }

    fn build_unary_operator(
        &mut self,
        op: Operator,
        insn: &UnaryInstruction<ResolvedOperand>,
    ) -> Expression {
        Expression::Unary(op, Box::new(self.build_expression(&insn.operand)))
    }

    fn build_expression(&mut self, op: &ResolvedOperand) -> Expression {
        let (op, id) = op;
        use ast::Expression::*;
        use il::ILOperand::*;
        match op {
            Pointer(_, _, _, _) => Dereference(Box::new(VarRef(Var::Var(self.get_var_name(*id))))),
            Register(_) => VarRef(Var::Var(self.get_var_name(*id))),
            Argument(offset) => VarRef(Var::Var(self.parameter_names[offset].clone())),
            Local(offset) => VarRef(Var::Var(self.local_names[offset].clone())),
            ILOperand::Value(value) => Expression::Value(*value),
        }
    }

    fn build_var(&mut self, op: &ResolvedOperand) -> Var {
        let (op, id) = op;
        use il::ILOperand::*;
        match op {
            Register(_) => Var::Var(self.get_var_name(*id)),
            Argument(offset) => Var::Var(self.parameter_names[offset].clone()),
            Local(offset) => Var::Var(self.local_names[offset].clone()),
            _ => panic!("Not supported"),
        }
    }

    fn get_var_name(&mut self, id: Option<i32>) -> String {
        match id {
            Some(id) => {
                let name = self.variable_names.get(&id);
                match name {
                    Some(name) => name.clone(),
                    None => {
                        let mut name = String::from("var");
                        name += &self.next_var_name_idx.to_string();
                        self.next_var_name_idx += 1;
                        self.variable_names.insert(id, name.clone());
                        self.types
                            .insert(name.clone(), self.variable_types[id as usize].clone());
                        name
                    }
                }
            }
            None => panic!("Invalid id"),
        }
    }
}
