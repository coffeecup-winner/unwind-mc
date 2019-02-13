use std::collections::{BTreeMap, HashMap, HashSet};

use asm::{InstructionGraph, LinkType};
use common::*;
use disassembler::*;
use il::*;

struct ILDecompiler {
    pub stack_offset: i32,
    pub frame_pointer_offset: i32,
}

pub fn decompile(graph: &InstructionGraph, address: u64) -> UResult<Vec<ILInstruction<ILOperand>>> {
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

        let il_insns = decompiler.convert_instruction(graph, &insn)?;
        if il_insns.len() > insn.length as usize {
            return Err(String::from("FIXME: not enough virtual addresses"));
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
    Ok(res)
}

impl ILDecompiler {
    // TODO: This code should be rewritten in a more robust way
    fn find_condition(
        &mut self,
        graph: &InstructionGraph,
        mut addr: u64,
        branch_type: BranchType,
    ) -> UResult<Option<BinaryInstruction<ILOperand>>> {
        use il::ILOperand::*;

        if branch_type == BranchType::Unconditional {
            return Ok(None);
        }

        while addr > 0 {
            if graph.contains_address(addr) {
                let insn = graph.get_vertex(&addr);
                match insn.code {
                    Mnemonic::Icmp => {
                        let (left, right) = self.get_binary_operands(&insn.operands)?;
                        return Ok(Some(binary(left, right)));
                    }
                    Mnemonic::Idec => {
                        let operand = self.get_unary_operand(&insn.operands)?;
                        return Ok(Some(binary(operand, Value(0))));
                    }
                    Mnemonic::Itest => {
                        let (left, right) = self.get_binary_operands(&insn.operands)?;
                        match (&left, &right) {
                            (Register(reg_left), Register(reg_right)) if reg_left == reg_right => {
                                return Ok(Some(binary(left, Value(0))));
                            }
                            _ => return Err(format!("Unsupported instruction\n{:#?}", insn)),
                        }
                    }
                    _ => {}
                }
            }
            addr -= 1;
        }
        Err(String::from("Failed to find a condition"))
    }

    fn convert_instruction(
        &mut self,
        graph: &InstructionGraph,
        insn: &Insn,
    ) -> Result<Vec<ILInstruction<ILOperand>>, String> {
        use il::BranchType::*;
        use il::ILBinaryOperator::*;
        use il::ILInstruction::*;
        use il::ILOperand::*;
        use il::ILUnaryOperator::*;
        match insn.code {
            Mnemonic::Iadd => {
                let (left, right) = self.get_binary_operands(&insn.operands)?;
                Ok(vec![Binary(Add, binary(left, right))])
            }
            Mnemonic::Iand => {
                let (left, right) = self.get_binary_operands(&insn.operands)?;
                Ok(vec![Binary(And, binary(left, right))])
            }
            Mnemonic::Icall => {
                let operand = self.get_unary_operand(&insn.operands)?;
                Ok(vec![Call(unary(operand))])
            }
            Mnemonic::Icdq => {
                Ok(vec![]) // TODO: take into account size extension
            }
            Mnemonic::Icmovl => {
                let (left, right) = self.get_binary_operands(&insn.operands)?;
                Ok(vec![
                    Branch(branch(
                        GreaterOrEqual,
                        self.find_condition(graph, insn.address, GreaterOrEqual)?,
                        insn.address + u64::from(insn.length),
                    )),
                    Assign(binary(left, right)),
                ])
            }
            Mnemonic::Icmp => Ok(vec![Nop]),
            Mnemonic::Idec => {
                let operand = self.get_unary_operand(&insn.operands)?;
                Ok(vec![Binary(Subtract, binary(operand, Value(1)))])
            }
            Mnemonic::Iidiv => {
                let operand = self.get_unary_operand(&insn.operands)?;
                Ok(vec![Binary(Divide, binary(Register(Reg::EAX), operand))])
            }
            Mnemonic::Iimul => {
                let (left, right) = self.get_binary_operands(&insn.operands)?;
                Ok(vec![Binary(Multiply, binary(left, right))])
            }
            Mnemonic::Iinc => {
                let operand = self.get_unary_operand(&insn.operands)?;
                Ok(vec![Binary(Add, binary(operand, Value(1)))])
            }
            Mnemonic::Ija | Mnemonic::Ijg => Ok(vec![Branch(branch(
                Greater,
                self.find_condition(graph, insn.address, Greater)?,
                insn.get_target_address(),
            ))]),
            Mnemonic::Ijae | Mnemonic::Ijge => Ok(vec![Branch(branch(
                GreaterOrEqual,
                self.find_condition(graph, insn.address, GreaterOrEqual)?,
                insn.get_target_address(),
            ))]),
            Mnemonic::Ijb | Mnemonic::Ijl => Ok(vec![Branch(branch(
                Less,
                self.find_condition(graph, insn.address, Less)?,
                insn.get_target_address(),
            ))]),
            Mnemonic::Ijbe | Mnemonic::Ijle => Ok(vec![Branch(branch(
                LessOrEqual,
                self.find_condition(graph, insn.address, LessOrEqual)?,
                insn.get_target_address(),
            ))]),
            Mnemonic::Ijmp => Ok(vec![Branch(branch(
                Unconditional,
                None,
                insn.get_target_address(),
            ))]),
            Mnemonic::Ijs => Ok(vec![Branch(branch(
                Less,
                self.find_condition(graph, insn.address, Less)?,
                insn.get_target_address(),
            ))]),
            Mnemonic::Ijns => Ok(vec![Branch(branch(
                GreaterOrEqual,
                self.find_condition(graph, insn.address, GreaterOrEqual)?,
                insn.get_target_address(),
            ))]),
            Mnemonic::Ijz => Ok(vec![Branch(branch(
                Equal,
                self.find_condition(graph, insn.address, Equal)?,
                insn.get_target_address(),
            ))]),
            Mnemonic::Ijnz => Ok(vec![Branch(branch(
                NotEqual,
                self.find_condition(graph, insn.address, NotEqual)?,
                insn.get_target_address(),
            ))]),
            Mnemonic::Ilea => {
                let (left, right) = self.get_binary_operands(&insn.operands)?;
                Ok(vec![Binary(LoadAddress, binary(left, right))])
            }
            Mnemonic::Imov => {
                let (left, right) = self.get_binary_operands(&insn.operands)?;
                match (&left, &right) {
                    (Register(Reg::EBP), Register(Reg::ESP)) => {
                        self.frame_pointer_offset = self.stack_offset;
                        Ok(vec![])
                    }
                    (Register(Reg::ESP), Register(Reg::EBP)) => {
                        self.stack_offset = self.frame_pointer_offset;
                        Ok(vec![])
                    }
                    (Register(Reg::ESP), _) => Err(String::from("Setting register ESP is not supported")),
                    _ => Ok(vec![Assign(binary(left, right))]),
                }
            }
            Mnemonic::Ineg => {
                let operand = self.get_unary_operand(&insn.operands)?;
                Ok(vec![Unary(Negate, unary(operand))])
            }
            Mnemonic::Inot => {
                let operand = self.get_unary_operand(&insn.operands)?;
                Ok(vec![Unary(Not, unary(operand))])
            }
            Mnemonic::Ior => {
                let (left, right) = self.get_binary_operands(&insn.operands)?;
                Ok(vec![Binary(Or, binary(left, right))])
            }
            Mnemonic::Ipush => {
                self.stack_offset -= REGISTER_SIZE as i32;
                Ok(vec![Nop])
            }
            Mnemonic::Ipop => {
                self.stack_offset += REGISTER_SIZE as i32;
                Ok(vec![Nop])
            }
            Mnemonic::Iret => {
                self.stack_offset += REGISTER_SIZE as i32;
                if self.stack_offset != 0 {
                    Err(String::from("Stack imbalance"))
                } else {
                    Ok(vec![Return(unary(Register(Reg::EAX)))])
                }
            }
            Mnemonic::Ishl => {
                let (left, right) = self.get_binary_operands(&insn.operands)?;
                Ok(vec![Binary(ShiftLeft, binary(left, right))])
            }
            Mnemonic::Isar => {
                let (left, right) = self.get_binary_operands(&insn.operands)?;
                Ok(vec![Binary(ShiftRight, binary(left, right))])
            }
            Mnemonic::Isub => {
                let (left, right) = self.get_binary_operands(&insn.operands)?;
                Ok(vec![Binary(Subtract, binary(left, right))])
            }
            Mnemonic::Itest => Ok(vec![Nop]),
            Mnemonic::Ixor => {
                let (left, right) = self.get_binary_operands(&insn.operands)?;
                Ok(vec![Binary(Xor, binary(left, right))])
            }
            _ => Err(format!("Unsupported instruction\n{:#?}", insn)),
        }
    }

    fn get_unary_operand(&mut self, operands: &[Operand]) -> UResult<ILOperand> {
        let mut ops = self.convert_operands(operands)?;
        if ops.len() != 1 {
            return Err(String::from("Invalid number of operands"))
        }
        Ok(ops.remove(0))
    }

    fn get_binary_operands(&mut self, operands: &[Operand]) -> UResult<(ILOperand, ILOperand)> {
        let mut ops = self.convert_operands(operands)?;
        if ops.len() != 2 {
            return Err(String::from("Invalid number of operands"))
        }
        let right = ops.remove(1);
        let left = ops.remove(0);
        Ok((left, right))
    }

    fn convert_operands(&mut self, operands: &[Operand]) -> UResult<Vec<ILOperand>> {
        operands.iter().map(|op| self.convert_operand(op)).collect()
    }

    fn convert_operand(&self, operand: &Operand) -> UResult<ILOperand> {
        use il::ILOperand::*;
        match operand {
            &Operand::Register(_, r) => Ok(Register(r)),
            &Operand::MemoryAbsolute(_, v) => Ok(Pointer(Reg::NONE, Reg::NONE, 0, v as u32)),
            &Operand::MemoryRelative(_, _, base, index, scale, offset) => {
                if base == Reg::ESP {
                    let offset = self.stack_offset + offset as i32;
                    if offset >= 0 {
                        Ok(Argument(offset))
                    } else {
                        Ok(Local(offset))
                    }
                } else if base == Reg::EBP {
                    let offset = self.frame_pointer_offset + offset as i32;
                    if offset >= 0 {
                        Ok(Argument(offset))
                    } else {
                        Ok(Local(offset))
                    }
                } else {
                    Ok(Pointer(base, index, scale, offset as u32))
                }
            }
            &Operand::Pointer(_, _, _) => Err(format!("Not supported: {:?}", operand)),
            &Operand::ImmediateSigned(_, v) => Ok(Value(v as i32)),
            &Operand::ImmediateUnsigned(_, v) => Ok(Value(v as i32)),
            &Operand::RelativeAddress(v) => Ok(Value(v as i32)),
            &Operand::Const(_, v) => Ok(Value(v as i32)),
        }
    }
}
