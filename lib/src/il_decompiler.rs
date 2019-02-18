use std::collections::{BTreeMap, HashMap, HashSet};

use common::*;
use disassembler::*;
use il::*;
use instruction_graph::{InstructionGraph, LinkType};
use project::{CallingConvention, Function};

struct ILDecompiler {
    pub stack_offset: i32,
    pub frame_pointer_offset: i32,
}

pub fn decompile(
    graph: &InstructionGraph,
    functions: &BTreeMap<u64, Function>,
    address: u64,
) -> UResult<Vec<ILInstruction<ILOperand>>> {
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

        let il_insns = decompiler.convert_instruction(graph, functions, &insn)?;
        il.insert(insn.address, il_insns);

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
    let mut index = 0;
    for (addr, insns) in il.iter() {
        targets.insert(*addr, index);
        index += insns.len() as u64;
    }

    let mut res = vec![];
    for (_, insns) in il {
        for insn in insns {
            match insn {
                Branch(branch) => {
                    res.push(Branch(BranchInstruction {
                        type_: branch.type_.clone(),
                        condition: branch.condition.clone(),
                        target: targets[&branch.target],
                    }));
                }
                insn => res.push(insn),
            }
        }
    }
    Ok(res)
}

impl ILDecompiler {
    fn find_condition(
        &mut self,
        graph: &InstructionGraph,
        mut addr: u64,
        flag: Flags,
    ) -> UResult<Option<BinaryInstruction<ILOperand>>> {
        use il::ILOperand::*;
        while addr > 0 {
            if graph.contains_address(addr) {
                let insn = graph.get_vertex(&addr);
                match insn.code {
                    Mnemonic::Iand | Mnemonic::Ior => {
                        if Self::check_flags(flag, Flags::PZS, Flags::CAO)? {
                            let (left, _) = self.get_binary_operands(&insn.operands)?;
                            return Ok(Some(binary(left, Value(0))));
                        }
                    }
                    Mnemonic::Iadd => {
                        if Self::check_flags(flag, Flags::CPAZSO, Flags::NONE)? {
                            let (left, _) = self.get_binary_operands(&insn.operands)?;
                            return Ok(Some(binary(left, Value(0))));
                        }
                    }
                    Mnemonic::Icmp | Mnemonic::Isub => {
                        if Self::check_flags(flag, Flags::CPAZSO, Flags::NONE)? {
                            let (left, right) = self.get_binary_operands(&insn.operands)?;
                            return Ok(Some(binary(left, right)));
                        }
                    }
                    Mnemonic::Idec | Mnemonic::Iinc => {
                        if Self::check_flags(flag, Flags::PAZSO, Flags::NONE)? {
                            let operand = self.get_unary_operand(&insn.operands)?;
                            return Ok(Some(binary(operand, Value(0))));
                        }
                    }
                    Mnemonic::Itest => {
                        if Self::check_flags(flag, Flags::PZS, Flags::CAO)? {
                            let (left, right) = self.get_binary_operands(&insn.operands)?;
                            match (&left, &right) {
                                (Register(reg_left), Register(reg_right))
                                    if reg_left == reg_right =>
                                {
                                    return Ok(Some(binary(left, Value(0))));
                                }
                                _ => return Err(format!("Unsupported instruction\n{:#?}", insn)),
                            }
                        }
                    }
                    Mnemonic::Ija
                    | Mnemonic::Ijae
                    | Mnemonic::Ijb
                    | Mnemonic::Ijbe
                    | Mnemonic::Ijg
                    | Mnemonic::Ijge
                    | Mnemonic::Ijl
                    | Mnemonic::Ijle
                    | Mnemonic::Ijs
                    | Mnemonic::Ijns
                    | Mnemonic::Ijz
                    | Mnemonic::Ijnz
                    | Mnemonic::Ijmp
                    | Mnemonic::Icmovl
                    | Mnemonic::Imov
                    | Mnemonic::Ipush
                    | Mnemonic::Ipop
                    | Mnemonic::Ilea => {}
                    Mnemonic::Iret => return Err(String::from("Failed to find condition")),
                    _ => {
                        return Err(format!(
                            "Unsupported instruction for condition searching: {:?}",
                            insn
                        ));
                    }
                }
            }
            addr -= 1;
        }
        Err(String::from("Failed to find a condition"))
    }

    fn check_flags(test: Flags, set_flags: Flags, invalid_flags: Flags) -> UResult<bool> {
        if (test & invalid_flags) != Flags::NONE {
            Err(String::from("Depending on undefined or constant flags"))
        } else if (test & set_flags) == test {
            Ok(true)
        } else if (test & set_flags) != Flags::NONE {
            Err(String::from(
                "Depending on flags set by different instructions",
            ))
        } else {
            Ok(false)
        }
    }

    fn convert_instruction(
        &mut self,
        graph: &InstructionGraph,
        functions: &BTreeMap<u64, Function>,
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
                match (&left, &right) {
                    (Register(Reg::ESP), Value(v)) => {
                        self.stack_offset += v;
                        Ok(vec![])
                    }
                    _ => Ok(vec![Binary(Add, binary(left, right))]),
                }
            }
            Mnemonic::Iand => {
                let (left, right) = self.get_binary_operands(&insn.operands)?;
                Ok(vec![Binary(And, binary(left, right))])
            }
            Mnemonic::Icall => {
                let operand = self.get_unary_operand(&insn.operands)?;
                match operand {
                    ILOperand::Value(v) => match functions.get(&(v as u64)) {
                        Some(func) if func.calling_convention == CallingConvention::Stdcall => {
                            match func.arguments_size {
                                Some(size) => {
                                    self.stack_offset += size as i32;
                                }
                                None => {
                                    return Err(format!(
                                        "Function {} is stdcall but has no arguments size set",
                                        func.name
                                    ));
                                }
                            }
                        }
                        Some(func) => {
                            warn!(
                                "Calling convention of function {} is not stdcall, skipping",
                                func.name
                            );
                        }
                        None => {
                            return Err(format!("Calling unknown function at address 0x{:x}", v));
                        }
                    },
                    _ => {
                        warn!("Calling function with unsupported operand {:?}", operand);
                    }
                }
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
                        self.find_condition(graph, insn.address, Flags::SF | Flags::OF)?,
                        insn.address + u64::from(insn.length),
                    )),
                    Assign(binary(left, right)),
                ])
            }
            Mnemonic::Icmp => Ok(vec![]),
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
            Mnemonic::Ija => Ok(vec![Branch(branch(
                Greater,
                self.find_condition(graph, insn.address, Flags::CF | Flags::ZF)?,
                self.get_target_address(insn)?,
            ))]),
            Mnemonic::Ijae => Ok(vec![Branch(branch(
                GreaterOrEqual,
                self.find_condition(graph, insn.address, Flags::CF)?,
                self.get_target_address(insn)?,
            ))]),
            Mnemonic::Ijb => Ok(vec![Branch(branch(
                Less,
                self.find_condition(graph, insn.address, Flags::CF)?,
                self.get_target_address(insn)?,
            ))]),
            Mnemonic::Ijbe => Ok(vec![Branch(branch(
                LessOrEqual,
                self.find_condition(graph, insn.address, Flags::CF | Flags::ZF)?,
                self.get_target_address(insn)?,
            ))]),
            Mnemonic::Ijg => Ok(vec![Branch(branch(
                Greater,
                self.find_condition(graph, insn.address, Flags::ZF | Flags::SF | Flags::OF)?,
                self.get_target_address(insn)?,
            ))]),
            Mnemonic::Ijge => Ok(vec![Branch(branch(
                GreaterOrEqual,
                self.find_condition(graph, insn.address, Flags::SF | Flags::OF)?,
                self.get_target_address(insn)?,
            ))]),
            Mnemonic::Ijl => Ok(vec![Branch(branch(
                Less,
                self.find_condition(graph, insn.address, Flags::SF | Flags::OF)?,
                self.get_target_address(insn)?,
            ))]),
            Mnemonic::Ijle => Ok(vec![Branch(branch(
                LessOrEqual,
                self.find_condition(graph, insn.address, Flags::ZF | Flags::SF | Flags::OF)?,
                self.get_target_address(insn)?,
            ))]),
            Mnemonic::Ijmp => Ok(vec![Branch(branch(
                Unconditional,
                None,
                self.get_target_address(insn)?,
            ))]),
            Mnemonic::Ijs => Ok(vec![Branch(branch(
                Less,
                self.find_condition(graph, insn.address, Flags::SF)?,
                self.get_target_address(insn)?,
            ))]),
            Mnemonic::Ijns => Ok(vec![Branch(branch(
                GreaterOrEqual,
                self.find_condition(graph, insn.address, Flags::SF)?,
                self.get_target_address(insn)?,
            ))]),
            Mnemonic::Ijz => Ok(vec![Branch(branch(
                Equal,
                self.find_condition(graph, insn.address, Flags::ZF)?,
                self.get_target_address(insn)?,
            ))]),
            Mnemonic::Ijnz => Ok(vec![Branch(branch(
                NotEqual,
                self.find_condition(graph, insn.address, Flags::ZF)?,
                self.get_target_address(insn)?,
            ))]),
            Mnemonic::Ilea => {
                let (left, right) = self.get_binary_operands(&insn.operands)?;
                Ok(vec![Binary(LoadAddress, binary(left, right))])
            }
            Mnemonic::Ilodsb => {
                if insn.operands.len() != 0 || insn.prefix_rep {
                    Err(format!("Unsupported lodsb instruction: {:?}", insn))
                } else {
                    // TODO: take into account register size
                    Ok(vec![
                        Assign(binary(
                            Register(Reg::EAX),
                            Pointer(Reg::ESI, Reg::NONE, 0, 0),
                        )),
                        Binary(Add, binary(Register(Reg::ESI), Value(1))),
                    ])
                }
            }
            Mnemonic::Ilodsw => {
                if insn.operands.len() != 0 || insn.prefix_rep {
                    Err(format!("Unsupported lodsw instruction: {:?}", insn))
                } else {
                    // TODO: take into account register size
                    Ok(vec![
                        Assign(binary(
                            Register(Reg::EAX),
                            Pointer(Reg::ESI, Reg::NONE, 0, 0),
                        )),
                        Binary(Add, binary(Register(Reg::ESI), Value(2))),
                    ])
                }
            }
            Mnemonic::Ilodsd => {
                if insn.operands.len() != 0 || insn.prefix_rep {
                    Err(format!("Unsupported lodsd instruction: {:?}", insn))
                } else {
                    // TODO: take into account register size
                    Ok(vec![
                        Assign(binary(
                            Register(Reg::EAX),
                            Pointer(Reg::ESI, Reg::NONE, 0, 0),
                        )),
                        Binary(Add, binary(Register(Reg::ESI), Value(4))),
                    ])
                }
            }
            Mnemonic::Iloop => Ok(vec![
                Binary(Subtract, binary(Register(Reg::ECX), Value(1))),
                Branch(branch(
                    Greater,
                    Some(binary(Register(Reg::ECX), Value(0))),
                    self.get_target_address(insn)?,
                )),
            ]),
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
                    (Register(Reg::ESP), _) => {
                        Err(String::from("Setting register ESP is not supported"))
                    }
                    _ => Ok(vec![Assign(binary(left, right))]),
                }
            }
            Mnemonic::Imovsd => {
                if !insn.prefix_str || !insn.prefix_rep {
                    Err(format!("Unsupported movsd instruction: {:?}", insn))
                } else {
                    Ok(vec![Copy(copy(
                        Register(Reg::EDI),
                        Register(Reg::ESI),
                        Value(4),
                        Register(Reg::ECX),
                    ))])
                }
            }
            Mnemonic::Imovsx => {
                let (left, right) = self.get_binary_operands(&insn.operands)?;
                Ok(vec![Assign(binary(left, right))]) // TODO: take into account size extension
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
                Ok(vec![])
            }
            Mnemonic::Ipop => {
                self.stack_offset += REGISTER_SIZE as i32;
                Ok(vec![])
            }
            Mnemonic::Iret => {
                self.stack_offset += REGISTER_SIZE as i32;
                if self.stack_offset != 0 {
                    Err(format!(
                        "Stack imbalance (stack offset is {})",
                        self.stack_offset
                    ))
                } else {
                    Ok(vec![Return(unary(Register(Reg::EAX)))])
                }
            }
            Mnemonic::Isar => {
                let (left, right) = self.get_binary_operands(&insn.operands)?;
                // TODO: take into account signedness
                Ok(vec![Binary(ShiftRight, binary(left, right))])
            }
            Mnemonic::Ishl => {
                let (left, right) = self.get_binary_operands(&insn.operands)?;
                Ok(vec![Binary(ShiftLeft, binary(left, right))])
            }
            Mnemonic::Ishr => {
                let (left, right) = self.get_binary_operands(&insn.operands)?;
                // TODO: take into account signedness
                Ok(vec![Binary(ShiftRight, binary(left, right))])
            }
            Mnemonic::Istosb => {
                if insn.operands.len() != 0 || insn.prefix_rep {
                    Err(format!("Unsupported stosb instruction: {:?}", insn))
                } else {
                    // TODO: take into account register size
                    Ok(vec![
                        Assign(binary(
                            Pointer(Reg::EDI, Reg::NONE, 0, 0),
                            Register(Reg::EAX),
                        )),
                        Binary(Add, binary(Register(Reg::EDI), Value(1))),
                    ])
                }
            }
            Mnemonic::Istosw => {
                if insn.operands.len() != 0 || insn.prefix_rep {
                    Err(format!("Unsupported stosw instruction: {:?}", insn))
                } else {
                    // TODO: take into account register size
                    Ok(vec![
                        Assign(binary(
                            Pointer(Reg::EDI, Reg::NONE, 0, 0),
                            Register(Reg::EAX),
                        )),
                        Binary(Add, binary(Register(Reg::EDI), Value(2))),
                    ])
                }
            }
            Mnemonic::Istosd => {
                if insn.operands.len() != 0 {
                    Err(format!("Unsupported stosd instruction: {:?}", insn))
                } else if insn.prefix_rep {
                    Ok(vec![Copy(copy(
                        Register(Reg::EDI),
                        Register(Reg::EAX),
                        Value(4),
                        Register(Reg::ECX),
                    ))])
                } else {
                    // TODO: take into account register size
                    Ok(vec![
                        Assign(binary(
                            Pointer(Reg::EDI, Reg::NONE, 0, 0),
                            Register(Reg::EAX),
                        )),
                        Binary(Add, binary(Register(Reg::EDI), Value(4))),
                    ])
                }
            }
            Mnemonic::Isub => {
                let (left, right) = self.get_binary_operands(&insn.operands)?;
                match (&left, &right) {
                    (Register(Reg::ESP), Value(v)) => {
                        self.stack_offset -= v;
                        Ok(vec![])
                    }
                    _ => Ok(vec![Binary(Subtract, binary(left, right))]),
                }
            }
            Mnemonic::Itest => Ok(vec![]),
            Mnemonic::Ixor => {
                let (left, right) = self.get_binary_operands(&insn.operands)?;
                Ok(vec![Binary(Xor, binary(left, right))])
            }
            _ => Err(format!("Unsupported instruction\n{:#?}", insn)),
        }
    }

    fn get_target_address(&self, insn: &Insn) -> UResult<u64> {
        if let Operand::CodeAddress(address) = insn.operands[0] {
            Ok(address)
        } else {
            Err(String::from("Invalid instruction"))
        }
    }

    fn get_unary_operand(&mut self, operands: &[Operand]) -> UResult<ILOperand> {
        let mut ops = self.convert_operands(operands)?;
        if ops.len() != 1 {
            return Err(String::from("Invalid number of operands"));
        }
        Ok(ops.remove(0))
    }

    fn get_binary_operands(&mut self, operands: &[Operand]) -> UResult<(ILOperand, ILOperand)> {
        let mut ops = self.convert_operands(operands)?;
        if ops.len() != 2 {
            return Err(String::from("Invalid number of operands"));
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
            &Operand::CodeAddress(v) => Ok(Value(v as i32)),
            &Operand::Const(_, v) => Ok(Value(v as i32)),
        }
    }
}
