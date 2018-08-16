use libudis86_sys::{ud_mnemonic_code::*, ud_type::*};
use std::collections::{BTreeMap, HashMap, HashSet};

use asm::{InstructionGraph, LinkType};
use common::*;
use il::*;
use udis86::*;

struct ILDecompiler {
    pub stack_offset: i32,
    pub frame_pointer_offset: i32,
    pub prev_insn: Option<Insn>,
}

pub fn decompile(graph: &InstructionGraph, address: u64) -> Vec<ILInstruction<ILOperand>> {
    let mut decompiler = ILDecompiler {
        stack_offset: -(REGISTER_SIZE as i32),
        frame_pointer_offset: 0,
        prev_insn: None,
    };
    let mut il = BTreeMap::new();
    let mut stack = vec![address];
    let mut visited = HashSet::new();
    while stack.len() > 0 {
        let address = stack.pop().unwrap();
        let insn = graph.get_vertex(&address).clone();

        let il_insns = decompiler.convert_instruction(&insn);
        if il_insns.len() > insn.length as usize {
            panic!("FIXME: not enough virtual addresses");
        }
        let mut i = 0;
        for il_insn in il_insns {
            il.insert(insn.address + i as u64, il_insn);
            i += 1;
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

    let mut targets = HashMap::new();
    let mut i = 0;
    for (addr, _) in il.iter() {
        targets.insert(*addr, i);
        i += 1;
    }

    use il::ILInstruction::Branch;
    let mut res = vec![];
    for (_, insn) in il {
        match insn {
            Branch(branch) => {
                res.push(Branch(BranchInstruction {
                    type_: branch.type_,
                    target: targets[&branch.target],
                }));
            }
            insn => res.push(insn),
        }
    }
    res
}

impl ILDecompiler {
    fn convert_instruction(&mut self, insn: &Insn) -> Vec<ILInstruction<ILOperand>> {
        use il::BranchType::*;
        use il::ILBinaryOperator::*;
        use il::ILInstruction::*;
        use il::ILOperand::*;
        use il::ILUnaryOperator::*;
        let result = match insn.code {
            UD_Iadd => {
                let (left, right) = self.get_binary_operands(&insn.operands);
                vec![Binary(Add, binary(left, right))]
            }
            UD_Iand => {
                let (left, right) = self.get_binary_operands(&insn.operands);
                vec![Binary(And, binary(left, right))]
            }
            UD_Icall => {
                let operand = self.get_unary_operand(&insn.operands);
                vec![Call(unary(operand))]
            }
            UD_Icdq => {
                vec![] // TODO: take into account size extension
            }
            UD_Icmovl => {
                let (left, right) = self.get_binary_operands(&insn.operands);
                vec![
                    Branch(branch(GreaterOrEqual, insn.address + insn.length as u64)),
                    Assign(binary(left, right)),
                ]
            }
            UD_Icmp => {
                let (left, right) = self.get_binary_operands(&insn.operands);
                vec![Compare(binary(left, right))]
            }
            UD_Idec => {
                let operand = self.get_unary_operand(&insn.operands);
                vec![Binary(Subtract, binary(operand, Value(1)))]
            }
            UD_Iidiv => {
                let operand = self.get_unary_operand(&insn.operands);
                vec![Binary(Divide, binary(Register(UD_R_EAX), operand))]
            }
            UD_Iimul => {
                let (left, right) = self.get_binary_operands(&insn.operands);
                vec![Binary(Multiply, binary(left, right))]
            }
            UD_Iinc => {
                let operand = self.get_unary_operand(&insn.operands);
                vec![Binary(Add, binary(operand, Value(1)))]
            }
            UD_Ija | UD_Ijg => vec![Branch(branch(Greater, insn.get_target_address()))],
            UD_Ijae | UD_Ijge => vec![Branch(branch(GreaterOrEqual, insn.get_target_address()))],
            UD_Ijb | UD_Ijl => vec![Branch(branch(Less, insn.get_target_address()))],
            UD_Ijbe | UD_Ijle => vec![Branch(branch(LessOrEqual, insn.get_target_address()))],
            UD_Ijmp => vec![Branch(branch(Unconditional, insn.get_target_address()))],
            UD_Ijz => {
                let prev = self.prev_insn.take().unwrap();
                let cond = self.try_get_virtual_condition_insn(&prev);
                let branch = Branch(branch(Equal, insn.get_target_address()));
                match cond {
                    Some(insn) => vec![insn, branch],
                    None => vec![branch],
                }
            }
            UD_Ijnz => {
                let prev = self.prev_insn.take().unwrap();
                let cond = self.try_get_virtual_condition_insn(&prev);
                let branch = Branch(branch(NotEqual, insn.get_target_address()));
                match cond {
                    Some(insn) => vec![insn, branch],
                    None => vec![branch],
                }
            }
            UD_Imov => {
                let (left, right) = self.get_binary_operands(&insn.operands);
                match (&left, &right) {
                    (Register(UD_R_EBP), Register(UD_R_ESP)) => {
                        self.frame_pointer_offset = self.stack_offset;
                        vec![]
                    }
                    (Register(UD_R_ESP), Register(UD_R_EBP)) => {
                        self.stack_offset = self.frame_pointer_offset;
                        vec![]
                    }
                    (Register(UD_R_EBP), _) | (Register(UD_R_ESP), _) => panic!("Not supported"),
                    _ => vec![Assign(binary(left, right))],
                }
            }
            UD_Ineg => {
                let operand = self.get_unary_operand(&insn.operands);
                vec![Unary(Negate, unary(operand))]
            }
            UD_Inot => {
                let operand = self.get_unary_operand(&insn.operands);
                vec![Unary(Not, unary(operand))]
            }
            UD_Ior => {
                let (left, right) = self.get_binary_operands(&insn.operands);
                vec![Binary(Or, binary(left, right))]
            }
            UD_Ipush => {
                self.stack_offset -= REGISTER_SIZE as i32;
                vec![Nop]
            }
            UD_Ipop => {
                self.stack_offset += REGISTER_SIZE as i32;
                vec![Nop]
            }
            UD_Iret => {
                self.stack_offset += REGISTER_SIZE as i32;
                if self.stack_offset != 0 {
                    panic!("Stack imbalance");
                }
                vec![Return(unary(Register(UD_R_EAX)))]
            }
            UD_Ishl => {
                let (left, right) = self.get_binary_operands(&insn.operands);
                vec![Binary(ShiftLeft, binary(left, right))]
            }
            UD_Isar => {
                let (left, right) = self.get_binary_operands(&insn.operands);
                vec![Binary(ShiftRight, binary(left, right))]
            }
            UD_Isub => {
                let (left, right) = self.get_binary_operands(&insn.operands);
                vec![Binary(Subtract, binary(left, right))]
            }
            UD_Itest => {
                let (left, right) = self.get_binary_operands(&insn.operands);
                match (&left, &right) {
                    (Register(reg_left), Register(reg_right)) if reg_left == reg_right => {
                        vec![Compare(binary(left, Value(0)))]
                    }
                    _ => panic!(format!("Unsupported instruction\n{:#?}", insn)),
                }
            }
            UD_Ixor => {
                let (left, right) = self.get_binary_operands(&insn.operands);
                vec![Binary(Xor, binary(left, right))]
            }
            _ => panic!(format!("Unsupported instruction\n{:#?}", insn)),
        };
        // TODO: remove this clone()
        self.prev_insn = Some(insn.clone());
        result
    }

    fn get_unary_operand(&mut self, operands: &Vec<Operand>) -> ILOperand {
        let mut ops = self.convert_operands(operands);
        let operand = ops.remove(0);
        operand
    }

    fn get_binary_operands(&mut self, operands: &Vec<Operand>) -> (ILOperand, ILOperand) {
        let mut ops = self.convert_operands(operands);
        let right = ops.remove(1);
        let left = ops.remove(0);
        (left, right)
    }

    fn try_get_virtual_condition_insn(
        &mut self,
        prev_insn: &Insn,
    ) -> Option<ILInstruction<ILOperand>> {
        match prev_insn.code {
            UD_Icmp | UD_Itest => None,
            UD_Idec | UD_Imov => {
                // TODO: this line is a heuristic and might be wrong in some cases
                let mut operands = self.convert_operands(&prev_insn.operands);
                Some(ILInstruction::Compare(binary(
                    operands.remove(0),
                    ILOperand::Value(0),
                )))
            }
            _ => panic!("Not supported"),
        }
    }

    fn convert_operands(&mut self, operands: &Vec<Operand>) -> Vec<ILOperand> {
        operands.iter().map(|op| self.convert_operand(op)).collect()
    }

    fn convert_operand(&self, operand: &Operand) -> ILOperand {
        use il::ILOperand::*;
        match operand.type_ {
            UD_OP_REG => Register(operand.base),
            UD_OP_MEM => {
                if operand.base == UD_R_ESP {
                    let offset = self.stack_offset + operand.get_memory_offset() as i32;
                    if offset >= 0 {
                        Argument(offset)
                    } else {
                        Local(offset)
                    }
                } else if operand.base == UD_R_EBP {
                    let offset = self.frame_pointer_offset + operand.get_memory_offset() as i32;
                    if offset >= 0 {
                        Argument(offset)
                    } else {
                        Local(offset)
                    }
                } else if operand.index == UD_NONE {
                    Pointer(operand.base, operand.get_memory_offset() as i32)
                } else {
                    panic!("Not supported")
                }
            }
            UD_OP_CONST | UD_OP_IMM => Value(operand.get_i64() as i32),
            _ => panic!("Not supported"),
        }
    }
}
