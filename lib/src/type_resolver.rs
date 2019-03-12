use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

use common::REGISTER_SIZE;
use disassembler::Reg;
use flow_analyzer::*;
use il;
use il::*;

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum DataType {
    Int32,
    Function,
    Pointer(Box<DataType>),
}

impl DataType {
    pub fn size(&self) -> u32 {
        REGISTER_SIZE
    }

    pub fn is_function(&self) -> bool {
        match self {
            DataType::Function => true,
            DataType::Pointer(t) => t.is_function(),
            _ => false,
        }
    }
}

pub type ResolvedOperand = (ILOperand, Option<i32>);

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Result {
    pub parameter_types: Vec<DataType>,
    pub local_types: Vec<DataType>,
    pub variable_types: Vec<DataType>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum TypeBuilder {
    Fresh,
    Function,
    Pointer(Rc<RefCell<TypeBuilder>>),
}

impl TypeBuilder {
    pub fn build(&self) -> DataType {
        match self {
            TypeBuilder::Fresh => DataType::Int32,
            TypeBuilder::Function => DataType::Function,
            TypeBuilder::Pointer(tb) => DataType::Pointer(Box::new(tb.borrow().build())),
        }
    }
}

#[derive(Clone, Copy)]
enum SsaMarker {
    PushIds,
    JoinPoint,
}

pub struct TypeResolver {
    types: HashMap<ILOperand, Rc<RefCell<TypeBuilder>>>,
    parameter_types: HashMap<i32, Rc<RefCell<TypeBuilder>>>,
    local_types: HashMap<i32, Rc<RefCell<TypeBuilder>>>,
    variable_types: Vec<DataType>,
    current_ids: HashMap<ILOperand, i32>,
    pushed_ids: Vec<HashMap<ILOperand, i32>>,
    same_ids: HashMap<i32, Vec<i32>>,
    coalesced_ids: BTreeMap<i32, i32>,
    next_id: i32,
}

impl TypeResolver {
    pub fn resolve_types(blocks: &Vec<Block<ILOperand>>) -> (Vec<Block<ResolvedOperand>>, Result) {
        let mut resolver = TypeResolver {
            types: HashMap::new(),
            parameter_types: HashMap::new(),
            local_types: HashMap::new(),
            variable_types: vec![],
            current_ids: HashMap::new(),
            pushed_ids: vec![],
            same_ids: HashMap::new(),
            coalesced_ids: BTreeMap::new(),
            next_id: 0,
        };

        let returns_value = TypeResolver::function_returns_value(&blocks);
        let mut blocks = resolver.convert_blocks(blocks, returns_value);

        let mut parameter_types_sorted: Vec<_> = resolver.parameter_types.iter().collect();
        parameter_types_sorted[..].sort_by_key(|(&k, _)| k);
        let mut parameter_types = vec![];
        for (_, type_builder) in parameter_types_sorted {
            parameter_types.push(type_builder.borrow().build());
        }

        let mut local_types_sorted: Vec<_> = resolver.local_types.iter().collect();
        local_types_sorted[..].sort_by_key(|(&k, _)| -k);
        let mut local_types = vec![];
        for (_, type_builder) in local_types_sorted {
            local_types.push(type_builder.borrow().build());
        }

        while !resolver.types.is_empty() {
            let (&op, _) = resolver.types.iter().nth(0).unwrap();
            resolver.finalize_type(&op);
        }

        let coalesced_ids: Vec<_> = resolver.coalesced_ids.iter().rev().collect();
        for (&id_from, &id_to) in coalesced_ids {
            blocks = resolver.coalesce_blocks(id_from, id_to, blocks);
        }

        let result = Result {
            parameter_types,
            local_types,
            variable_types: resolver.variable_types,
        };
        (blocks, result)
    }

    fn coalesce_blocks(
        &self,
        id_from: i32,
        id_to: i32,
        blocks: Vec<Block<ResolvedOperand>>,
    ) -> Vec<Block<ResolvedOperand>> {
        blocks
            .into_iter()
            .map(|b| match b {
                Block::SequentialBlock(b) => Block::SequentialBlock(SequentialBlock {
                    instructions: self.coalesce_instructions(id_from, id_to, b.instructions),
                }),
                Block::WhileBlock(b) => Block::WhileBlock(WhileBlock {
                    condition: self.coalesce_instructions(id_from, id_to, b.condition),
                    body: self.coalesce_blocks(id_from, id_to, b.body),
                }),
                Block::DoWhileBlock(b) => Block::DoWhileBlock(DoWhileBlock {
                    condition: self.coalesce_instructions(id_from, id_to, b.condition),
                    body: self.coalesce_blocks(id_from, id_to, b.body),
                }),
                Block::ForBlock(b) => Block::ForBlock(ForBlock {
                    condition: self.coalesce_instructions(id_from, id_to, b.condition),
                    modifier: self.coalesce_instructions(id_from, id_to, b.modifier),
                    body: self.coalesce_blocks(id_from, id_to, b.body),
                }),
                Block::ConditionalBlock(b) => Block::ConditionalBlock(ConditionalBlock {
                    condition: self.coalesce_instructions(id_from, id_to, b.condition),
                    true_branch: self.coalesce_blocks(id_from, id_to, b.true_branch),
                    false_branch: self.coalesce_blocks(id_from, id_to, b.false_branch),
                }),
            })
            .collect()
    }

    fn coalesce_instructions(
        &self,
        id_from: i32,
        id_to: i32,
        insns: Vec<ILInstruction<ResolvedOperand>>,
    ) -> Vec<ILInstruction<ResolvedOperand>> {
        use il::ILInstruction::*;
        insns
            .into_iter()
            .map(|i| match i {
                Binary(op, binary) => Binary(op, self.coalesce_binary(id_from, id_to, &binary)),
                Unary(op, unary) => Unary(op, self.coalesce_unary(id_from, id_to, unary)),
                Call(unary) => Call(self.coalesce_unary(id_from, id_to, unary)),
                Return(unary) => Return(self.coalesce_unary(id_from, id_to, unary)),
                Assign(binary) => Assign(self.coalesce_binary(id_from, id_to, &binary)),
                Copy(_copy) => panic!("Not supported yet"),
                Continue => Continue,
                Break => Break,
                Branch(br) => Branch(branch(
                    br.type_,
                    br.condition
                        .map(|b| self.coalesce_binary(id_from, id_to, &b)),
                    br.target,
                )),
            })
            .collect()
    }

    fn coalesce_unary(
        &self,
        id_from: i32,
        id_to: i32,
        unary: UnaryInstruction<ResolvedOperand>,
    ) -> UnaryInstruction<ResolvedOperand> {
        let (op, op_id) = unary.operand;
        match op_id {
            Some(id) if id == id_from => il::unary((op, Some(id_to))),
            _ => unary,
        }
    }

    fn coalesce_binary(
        &self,
        id_from: i32,
        id_to: i32,
        binary: &BinaryInstruction<ResolvedOperand>,
    ) -> BinaryInstruction<ResolvedOperand> {
        let (left, left_id) = binary.left;
        let left_id = if left_id == Some(id_from) {
            Some(id_to)
        } else {
            left_id
        };
        let (right, right_id) = binary.right;
        let right_id = if right_id == Some(id_from) {
            Some(id_to)
        } else {
            right_id
        };
        il::binary((left, left_id), (right, right_id))
    }

    fn on_ssa_marker(&mut self, marker: SsaMarker) {
        match marker {
            SsaMarker::PushIds => {
                let copy = self.current_ids.clone();
                self.pushed_ids.push(copy);
            }
            SsaMarker::JoinPoint => {
                let ids1 = self.pushed_ids.pop().unwrap();
                let ids0 = self.pushed_ids.pop().unwrap();
                self.current_ids.clear();
                for (key, value) in ids0 {
                    self.current_ids.insert(key, value);
                }
                for (key, value) in ids1 {
                    match self.current_ids.get(&key) {
                        Some(&val) if val != value => {
                            match self.same_ids.get_mut(&val) {
                                Some(ids) => {
                                    ids.push(value);
                                }
                                None => {
                                    self.same_ids.insert(val, vec![value]);
                                }
                            };
                        }
                        Some(_) => (), // skip unchanged ids
                        None => {
                            self.current_ids.insert(key, value);
                        }
                    };
                }
            }
        }
    }

    fn convert_blocks(
        &mut self,
        blocks: &Vec<Block<ILOperand>>,
        returns_value: bool,
    ) -> Vec<Block<ResolvedOperand>> {
        use type_resolver::SsaMarker::*;
        blocks
            .iter()
            .map(|b| match b {
                Block::SequentialBlock(b) => Block::SequentialBlock(SequentialBlock {
                    instructions: self.convert_instructions(&b.instructions, returns_value),
                }),
                Block::WhileBlock(b) => {
                    self.on_ssa_marker(PushIds);
                    let condition = self.convert_instructions(&b.condition, returns_value);
                    let body = self.convert_blocks(&b.body, returns_value);
                    self.on_ssa_marker(PushIds);
                    self.on_ssa_marker(JoinPoint);
                    Block::WhileBlock(WhileBlock { condition, body })
                }
                Block::DoWhileBlock(b) => {
                    self.on_ssa_marker(PushIds);
                    let body = self.convert_blocks(&b.body, returns_value);
                    let condition = self.convert_instructions(&b.condition, returns_value);
                    self.on_ssa_marker(PushIds);
                    self.on_ssa_marker(JoinPoint);
                    Block::DoWhileBlock(DoWhileBlock { condition, body })
                }
                Block::ForBlock(b) => {
                    self.on_ssa_marker(PushIds);
                    let body = self.convert_blocks(&b.body, returns_value);
                    let condition = self.convert_instructions(&b.condition, returns_value);
                    let modifier = self.convert_instructions(&b.modifier, returns_value);
                    self.on_ssa_marker(PushIds);
                    self.on_ssa_marker(JoinPoint);
                    Block::ForBlock(ForBlock {
                        condition,
                        modifier,
                        body,
                    })
                }
                Block::ConditionalBlock(b) => {
                    let has_false_branch = !b.false_branch.is_empty();
                    let has_true_branch = !b.true_branch.is_empty();
                    let condition = self.convert_instructions(&b.condition, returns_value);
                    if has_false_branch != has_true_branch {
                        self.on_ssa_marker(PushIds);
                    }
                    let true_branch = if has_true_branch {
                        let true_branch = self.convert_blocks(&b.true_branch, returns_value);
                        self.on_ssa_marker(PushIds);
                        true_branch
                    } else {
                        vec![]
                    };
                    let false_branch = if has_false_branch {
                        let false_branch = self.convert_blocks(&b.false_branch, returns_value);
                        self.on_ssa_marker(PushIds);
                        false_branch
                    } else {
                        vec![]
                    };
                    self.on_ssa_marker(JoinPoint);
                    Block::ConditionalBlock(ConditionalBlock {
                        condition,
                        true_branch,
                        false_branch,
                    })
                }
            })
            .collect()
    }

    fn convert_instructions(
        &mut self,
        insns: &Vec<ILInstruction<ILOperand>>,
        returns_value: bool,
    ) -> Vec<ILInstruction<ResolvedOperand>> {
        use il::ILInstruction::*;
        insns
            .iter()
            .map(|i| match i {
                Binary(op, binary) => Binary(*op, self.convert_binary(&binary)),
                Unary(op, unary) => Unary(*op, self.convert_unary(&unary)),
                Assign(binary) => Assign(self.convert_assign(&binary)),
                Copy(_copy) => panic!("Not supported yet"),
                Call(unary) => Call(self.convert_call(&unary)),
                Return(unary) => Return(self.convert_return(&unary, returns_value)),
                Continue => Continue,
                Break => Break,
                Branch(br) => Branch(branch(
                    br.type_,
                    br.condition.clone().map(|b| self.convert_binary(&b)),
                    br.target,
                )),
            })
            .collect()
    }

    fn convert_unary(
        &mut self,
        unary: &UnaryInstruction<ILOperand>,
    ) -> UnaryInstruction<ResolvedOperand> {
        il::unary((unary.operand, self.get_current_id(&unary.operand)))
    }

    fn convert_binary(
        &mut self,
        binary: &BinaryInstruction<ILOperand>,
    ) -> BinaryInstruction<ResolvedOperand> {
        let right_id = self.get_current_id(&binary.right);
        match binary.right {
            ILOperand::Value(_) => {
                if !self.type_exists(&binary.left) {
                    self.assign_type_builder(&binary.left, &ILOperand::Value(0)); // get a fresh type
                }
            }
            _ => {
                if self.type_exists(&binary.right) && self.type_exists(&binary.left) {
                    if self.get_type(&binary.right) != self.get_type(&binary.left) {
                        panic!("Type mismatch");
                    }
                } else if self.type_exists(&binary.right) {
                    self.assign_type_builder(&binary.left, &binary.right);
                } else if self.type_exists(&binary.left) {
                    self.assign_type_builder(&binary.right, &binary.left);
                } else {
                    panic!("Not supported");
                }
            }
        };
        il::binary(
            (binary.left, self.get_current_id(&binary.left)),
            (binary.right, right_id),
        )
    }

    fn convert_assign(
        &mut self,
        binary: &BinaryInstruction<ILOperand>,
    ) -> BinaryInstruction<ResolvedOperand> {
        use il::ILOperand::*;
        let op = match binary.right {
            Pointer(reg, _, _, _) => Register(reg),
            _ => binary.right,
        };
        if !self.type_exists(&op) {
            match op {
                Argument(_) | Local(_) | Value(_) => (),
                _ => panic!("Right side of assignment does not have a type assigned"),
            };
        }

        let right_id = self.get_current_id(&op);
        self.assign_type_builder(&binary.left, &op);
        if let Pointer(_, _, _, _) = binary.right {
            let type_ = Rc::new(RefCell::new((*self.get_type(&op).borrow()).clone()));
            *self.get_type(&op).borrow_mut() = TypeBuilder::Pointer(type_.clone());
            self.set_type(&binary.left, type_);
        }
        il::binary(
            (binary.left, self.get_current_id(&binary.left)),
            (binary.right, right_id),
        )
    }

    fn convert_call(
        &mut self,
        unary: &UnaryInstruction<ILOperand>,
    ) -> UnaryInstruction<ResolvedOperand> {
        let op = unary.operand;
        *self.get_type(&op).borrow_mut() = TypeBuilder::Function;
        il::unary((op, self.get_current_id(&op)))
    }

    fn convert_return(
        &mut self,
        unary: &UnaryInstruction<ILOperand>,
        returns_value: bool,
    ) -> UnaryInstruction<ResolvedOperand> {
        let op = unary.operand;
        if returns_value {
            il::unary((op, self.get_current_id(&op)))
        } else {
            il::unary((op, None))
        }
    }

    fn function_returns_value(blocks: &[Block<ILOperand>]) -> bool {
        // This tests only the top-level scope, which is probably an incomplete heuristic,
        // most probably need to check all paths
        blocks.iter().any(|b| match b {
            Block::SequentialBlock(ref b) => b.instructions.iter().any(|i| match i {
                ILInstruction::Assign(ref i) if i.left == ILOperand::Register(Reg::EAX) => true,
                _ => false,
            }),
            _ => false,
        })
    }

    fn type_exists(&self, op: &ILOperand) -> bool {
        use il::ILOperand::*;
        match op {
            Argument(offset) => self.parameter_types.contains_key(offset),
            Local(offset) => self.local_types.contains_key(offset),
            _ => self.types.contains_key(op),
        }
    }

    fn assign_type_builder(&mut self, target: &ILOperand, source: &ILOperand) {
        use il::ILOperand::*;
        let type_builder = match source {
            Value(_) => Rc::new(RefCell::new(TypeBuilder::Fresh)),
            Argument(offset) => match self.parameter_types.get(&offset) {
                Some(type_) => type_.clone(),
                None => {
                    let type_ = Rc::new(RefCell::new(TypeBuilder::Fresh));
                    self.parameter_types.insert(*offset, type_.clone());
                    type_
                }
            },
            Local(offset) => match self.local_types.get(&offset) {
                Some(type_) => type_.clone(),
                None => {
                    let type_ = Rc::new(RefCell::new(TypeBuilder::Fresh));
                    self.local_types.insert(*offset, type_.clone());
                    type_
                }
            },
            _ => self.get_type(&source),
        };
        match target {
            Argument(offset) => {
                self.parameter_types.insert(*offset, type_builder);
            }
            Local(offset) => {
                self.local_types.insert(*offset, type_builder);
            }
            _ => {
                if self.types.get(&target).is_some() {
                    self.finalize_type(&target);
                }
                self.types.insert(*target, type_builder);
                self.current_ids.insert(*target, self.next_id);
                self.next_id += 1;
            }
        }
    }

    fn get_type(&self, op: &ILOperand) -> Rc<RefCell<TypeBuilder>> {
        match op {
            ILOperand::Argument(offset) => self.parameter_types[&offset].clone(),
            ILOperand::Local(offset) => self.local_types[&offset].clone(),
            _ => self.types[op].clone(),
        }
    }

    fn set_type(&mut self, op: &ILOperand, type_: Rc<RefCell<TypeBuilder>>) {
        match op {
            ILOperand::Argument(offset) => {
                self.parameter_types.insert(*offset, type_);
            }
            ILOperand::Local(offset) => {
                self.local_types.insert(*offset, type_);
            }
            _ => {
                self.types.insert(*op, type_);
            }
        }
    }

    fn finalize_type(&mut self, op: &ILOperand) {
        let id = self.get_current_id(op).unwrap();
        while self.variable_types.len() <= id as usize {
            self.variable_types.push(DataType::Int32);
        }
        self.variable_types[id as usize] = self.types[op].borrow().build();
        self.types.remove(op);
    }

    fn get_current_id(&mut self, op: &ILOperand) -> Option<i32> {
        match op {
            ILOperand::Register(_) | ILOperand::Pointer(_, _, _, _) => {
                let id = self.current_ids[op];
                self.coalesce_ids(id);
                Some(id)
            }
            _ => None,
        }
    }

    fn coalesce_ids(&mut self, id: i32) {
        let mut ids_to_coalesce = vec![];
        if let Some(ids) = self.same_ids.get(&id) {
            for &same_id in ids.iter() {
                match self.coalesced_ids.get(&same_id) {
                    Some(&v) if v == id => (),
                    Some(_) => panic!("Trying to coalesce with a new id"),
                    None => {
                        self.coalesced_ids.insert(same_id, id);
                        ids_to_coalesce.push(same_id);
                    }
                }
            }
        }
        for id in ids_to_coalesce {
            self.coalesce_ids(id);
        }
    }
}
