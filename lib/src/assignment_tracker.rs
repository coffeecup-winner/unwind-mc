use common::*;
use disassembler::*;
use instruction_graph::*;

pub struct AssignmentTracker;
impl AssignmentTracker {
    // NOTE: expects reversed graph
    pub fn find(
        graph: &InstructionGraph,
        address: u64,
        register: Reg,
        try_match: &mut FnMut(&Insn, Reg) -> bool,
    ) -> Option<u64> {
        let mut skipped_initial_instruction = false;
        let mut stack = vec![address];
        while !stack.is_empty() {
            let address = stack.pop().unwrap();

            for pair in graph.get_adjacent(&address).into_iter().rev() {
                if let Ok((addr, link)) = pair {
                    match link.type_ {
                        LinkType::Next | LinkType::Branch | LinkType::SwitchCaseJump => {
                            stack.push(*addr);
                        }
                        _ => (),
                    }
                }
            }

            if !skipped_initial_instruction {
                skipped_initial_instruction = true;
            }

            let insn = graph.get_vertex(&address);
            if try_match(insn, register) {
                match insn.operands[1] {
                    Operand::Register(_, r) => {
                        return AssignmentTracker::find(graph, insn.address, r, &mut |i, reg| {
                            if let Operand::Register(_, r) = i.operands[0] {
                                if r == reg && i.code == Mnemonic::Imov {
                                    return true;
                                }
                            }
                            return false;
                        });
                    }
                    Operand::ImmediateSigned(_, v) => return Some(v as u64),
                    Operand::ImmediateUnsigned(_, v) => return Some(v),
                    _ => {
                        // TODO: logger.Warn("Cannot track assignments from 0x{0:x8} operand type {1}", instr.Offset, instr.Operands.[1].Type)
                        return None;
                    }
                }
            }

            if graph.get_in_value(insn.address) > 1 {
                // TODO: logger.Warn("Cannot track assignments from 0x{0:x8}, the number of incoming links > 1", instr.Offset)
                return None;
            }

            if let Mnemonic::Imov = insn.code {
                match (insn.operands[0], insn.operands[1]) {
                    (Operand::Register(_, r0), Operand::Register(_, r1)) if r0 == register => {
                        return AssignmentTracker::find(graph, insn.address, r1, try_match);
                    }
                    _ => {}
                }
            }
        }
        None
    }
}
