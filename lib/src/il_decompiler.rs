use std::collections::{BTreeMap, HashMap, HashSet};

use asm::{InstructionGraph, LinkType};
use common::*;
use disassembler::*;
use il::*;

struct ILDecompiler {
    pub stack_offset: i32,
    pub frame_pointer_offset: i32,
}

pub fn decompile(graph: &InstructionGraph, address: u64) -> Vec<ILInstruction<ILOperand>> {
    let mut decompiler = ILDecompiler {
        stack_offset: -(REGISTER_SIZE as i32),
        frame_pointer_offset: 0,
    };
    let mut il = BTreeMap::new();
    let mut stack = vec![address];
    let mut visited = HashSet::new();
    while !stack.is_empty() {
        let address = stack.pop().unwrap();
        let insn = graph.get_vertex(&address).clone();

        let il_insns = decompiler.convert_instruction(graph, &insn);
        if il_insns.len() > insn.length as usize {
            panic!("FIXME: not enough virtual addresses");
        }
        for (i, il_insn) in il_insns.into_iter().enumerate() {
            il.insert(insn.address + i as u64, il_insn);
        }

        for pair in graph.get_adjacent(&address).into_iter().rev() {
            match pair {
                Err(_) => {}
                Ok((addr, link)) => match link.type_ {
                    LinkType::Next | LinkType::Branch | LinkType::SwitchCaseJump => {
                        if !visited.contains(addr) {
                            stack.push(*addr);
                            visited.insert(*addr);
                        }
                    }
                    _ => {}
                },
            }
        }
    }

    use il::ILInstruction::*;
    let mut targets = HashMap::new();
    let mut nop_counts = HashMap::new();
    let mut nops = 0;
    for (i, (addr, insn)) in il.iter().enumerate() {
        targets.insert(*addr, i as u64);
        nop_counts.insert(*addr, nops as u64);
        if let Nop = insn {
            nops += 1;
        }
    }

    let mut res = vec![];
    for (_, insn) in il {
        match insn {
            Branch(branch) => {
                res.push(Branch(BranchInstruction {
                    type_: branch.type_.clone(),
                    condition: branch.condition.clone(),
                    target: targets[&branch.target] - nop_counts[&branch.target],
                }));
            }
            Nop => {}
            insn => res.push(insn),
        }
    }
    res
}

impl ILDecompiler {
    fn find_condition(
        &mut self,
        graph: &InstructionGraph,
        mut addr: u64,
        branch_type: BranchType,
    ) -> Option<BinaryInstruction<ILOperand>> {
        use il::ILOperand::*;

        if branch_type == BranchType::Unconditional {
            return None;
        }

        while addr > 0 {
            if graph.contains_address(addr) {
                let insn = graph.get_vertex(&addr);
                match insn.code {
                    Mnemonic::Icmp => {
                        let (left, right) = self.get_binary_operands(&insn.operands);
                        return Some(binary(left, right));
                    }
                    Mnemonic::Idec => {
                        let operand = self.get_unary_operand(&insn.operands);
                        return Some(binary(operand, Value(0)));
                    }
                    Mnemonic::Itest => {
                        let (left, right) = self.get_binary_operands(&insn.operands);
                        match (&left, &right) {
                            (Register(reg_left), Register(reg_right)) if reg_left == reg_right => {
                                return Some(binary(left, Value(0)))
                            }
                            _ => panic!(format!("Unsupported instruction\n{:#?}", insn)),
                        }
                    }
                    _ => {}
                }
            }
            addr -= 1;
        }
        panic!("Failed to find a condition")
    }

    fn convert_instruction(
        &mut self,
        graph: &InstructionGraph,
        insn: &Insn,
    ) -> Vec<ILInstruction<ILOperand>> {
        use il::BranchType::*;
        use il::ILBinaryOperator::*;
        use il::ILInstruction::*;
        use il::ILOperand::*;
        use il::ILUnaryOperator::*;
        match insn.code {
            Mnemonic::Iadd => {
                let (left, right) = self.get_binary_operands(&insn.operands);
                vec![Binary(Add, binary(left, right))]
            }
            Mnemonic::Iand => {
                let (left, right) = self.get_binary_operands(&insn.operands);
                vec![Binary(And, binary(left, right))]
            }
            Mnemonic::Icall => {
                let operand = self.get_unary_operand(&insn.operands);
                vec![Call(unary(operand))]
            }
            Mnemonic::Icdq => {
                vec![] // TODO: take into account size extension
            }
            Mnemonic::Icmovl => {
                let (left, right) = self.get_binary_operands(&insn.operands);
                vec![
                    Branch(branch(
                        GreaterOrEqual,
                        self.find_condition(graph, insn.address, GreaterOrEqual),
                        insn.address + u64::from(insn.length),
                    )),
                    Assign(binary(left, right)),
                ]
            }
            Mnemonic::Icmp => vec![Nop],
            Mnemonic::Idec => {
                let operand = self.get_unary_operand(&insn.operands);
                vec![Binary(Subtract, binary(operand, Value(1)))]
            }
            Mnemonic::Iidiv => {
                let operand = self.get_unary_operand(&insn.operands);
                vec![Binary(Divide, binary(Register(Reg::EAX), operand))]
            }
            Mnemonic::Iimul => {
                let (left, right) = self.get_binary_operands(&insn.operands);
                vec![Binary(Multiply, binary(left, right))]
            }
            Mnemonic::Iinc => {
                let operand = self.get_unary_operand(&insn.operands);
                vec![Binary(Add, binary(operand, Value(1)))]
            }
            Mnemonic::Ija | Mnemonic::Ijg => vec![Branch(branch(
                Greater,
                self.find_condition(graph, insn.address, Greater),
                insn.get_target_address(),
            ))],
            Mnemonic::Ijae | Mnemonic::Ijge => vec![Branch(branch(
                GreaterOrEqual,
                self.find_condition(graph, insn.address, GreaterOrEqual),
                insn.get_target_address(),
            ))],
            Mnemonic::Ijb | Mnemonic::Ijl => vec![Branch(branch(
                Less,
                self.find_condition(graph, insn.address, Less),
                insn.get_target_address(),
            ))],
            Mnemonic::Ijbe | Mnemonic::Ijle => vec![Branch(branch(
                LessOrEqual,
                self.find_condition(graph, insn.address, LessOrEqual),
                insn.get_target_address(),
            ))],
            Mnemonic::Ijmp => vec![Branch(branch(
                Unconditional,
                None,
                insn.get_target_address(),
            ))],
            Mnemonic::Ijs => vec![Branch(branch(
                Less,
                self.find_condition(graph, insn.address, Less),
                insn.get_target_address(),
            ))],
            Mnemonic::Ijns => vec![Branch(branch(
                GreaterOrEqual,
                self.find_condition(graph, insn.address, GreaterOrEqual),
                insn.get_target_address(),
            ))],
            Mnemonic::Ijz => vec![Branch(branch(
                Equal,
                self.find_condition(graph, insn.address, Equal),
                insn.get_target_address(),
            ))],
            Mnemonic::Ijnz => vec![Branch(branch(
                NotEqual,
                self.find_condition(graph, insn.address, NotEqual),
                insn.get_target_address(),
            ))],
            Mnemonic::Ilea => {
                let (left, right) = self.get_binary_operands(&insn.operands);
                vec![Binary(LoadAddress, binary(left, right))]
            }
            Mnemonic::Imov => {
                let (left, right) = self.get_binary_operands(&insn.operands);
                match (&left, &right) {
                    (Register(Reg::EBP), Register(Reg::ESP)) => {
                        self.frame_pointer_offset = self.stack_offset;
                        vec![]
                    }
                    (Register(Reg::ESP), Register(Reg::EBP)) => {
                        self.stack_offset = self.frame_pointer_offset;
                        vec![]
                    }
                    (Register(Reg::EBP), _) | (Register(Reg::ESP), _) => panic!("Not supported"),
                    _ => vec![Assign(binary(left, right))],
                }
            }
            Mnemonic::Ineg => {
                let operand = self.get_unary_operand(&insn.operands);
                vec![Unary(Negate, unary(operand))]
            }
            Mnemonic::Inot => {
                let operand = self.get_unary_operand(&insn.operands);
                vec![Unary(Not, unary(operand))]
            }
            Mnemonic::Ior => {
                let (left, right) = self.get_binary_operands(&insn.operands);
                vec![Binary(Or, binary(left, right))]
            }
            Mnemonic::Ipush => {
                self.stack_offset -= REGISTER_SIZE as i32;
                vec![Nop]
            }
            Mnemonic::Ipop => {
                self.stack_offset += REGISTER_SIZE as i32;
                vec![Nop]
            }
            Mnemonic::Iret => {
                self.stack_offset += REGISTER_SIZE as i32;
                if self.stack_offset != 0 {
                    panic!("Stack imbalance");
                }
                vec![Return(unary(Register(Reg::EAX)))]
            }
            Mnemonic::Ishl => {
                let (left, right) = self.get_binary_operands(&insn.operands);
                vec![Binary(ShiftLeft, binary(left, right))]
            }
            Mnemonic::Isar => {
                let (left, right) = self.get_binary_operands(&insn.operands);
                vec![Binary(ShiftRight, binary(left, right))]
            }
            Mnemonic::Isub => {
                let (left, right) = self.get_binary_operands(&insn.operands);
                vec![Binary(Subtract, binary(left, right))]
            }
            Mnemonic::Itest => vec![Nop],
            Mnemonic::Ixor => {
                let (left, right) = self.get_binary_operands(&insn.operands);
                vec![Binary(Xor, binary(left, right))]
            }
            _ => panic!(format!("Unsupported instruction\n{:#?}", insn)),
        }
    }

    fn get_unary_operand(&mut self, operands: &[Operand]) -> ILOperand {
        let mut ops = self.convert_operands(operands);
        ops.remove(0)
    }

    fn get_binary_operands(&mut self, operands: &[Operand]) -> (ILOperand, ILOperand) {
        let mut ops = self.convert_operands(operands);
        let right = ops.remove(1);
        let left = ops.remove(0);
        (left, right)
    }

    fn convert_operands(&mut self, operands: &[Operand]) -> Vec<ILOperand> {
        operands.iter().map(|op| self.convert_operand(op)).collect()
    }

    fn convert_operand(&self, operand: &Operand) -> ILOperand {
        use il::ILOperand::*;
        match operand {
            &Operand::Register(_, r) => Register(r),
            &Operand::MemoryRelative(_, _, base, index, scale, offset) => {
                if base == Reg::ESP {
                    let offset = self.stack_offset + offset as i32;
                    if offset >= 0 {
                        Argument(offset)
                    } else {
                        Local(offset)
                    }
                } else if base == Reg::EBP {
                    let offset = self.frame_pointer_offset + offset as i32;
                    if offset >= 0 {
                        Argument(offset)
                    } else {
                        Local(offset)
                    }
                } else {
                    Pointer(base, index, scale, offset as i32)
                }
            }
            &Operand::ImmediateSigned(_, v) => Value(v as i32),
            &Operand::ImmediateUnsigned(_, v) => Value(v as i32),
            &Operand::Const(_, v) => Value(v as i32),
            _ => panic!("Not supported"),
        }
    }
}
