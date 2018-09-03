use libudis86_sys::{ud_lval, ud_mnemonic_code, ud_type};

use asm::*;
use common::*;
use udis86::*;

pub struct AssignmentTracker;
impl AssignmentTracker {
    // NOTE: expects reversed graph
    pub fn find(
        graph: &InstructionGraph,
        address: u64,
        register: ud_type,
        try_match: &mut FnMut(&Insn, ud_type) -> bool,
    ) -> Option<ud_lval> {
        let mut skipped_initial_instruction = false;
        let mut stack = vec![address];
        while stack.len() > 0 {
            let address = stack.pop().unwrap();

            for pair in graph.get_adjacent(&address).into_iter().rev() {
                match pair {
                    Ok((addr, link)) => match link.type_ {
                        LinkType::Next | LinkType::Branch | LinkType::SwitchCaseJump => {
                            stack.push(*addr);
                        }
                        _ => (),
                    },
                    _ => (),
                }
            }

            if !skipped_initial_instruction {
                skipped_initial_instruction = true;
            }

            let insn = graph.get_vertex(&address);
            if try_match(insn, register) {
                if insn.operands[1].type_ == ud_type::UD_OP_REG {
                    return AssignmentTracker::find(
                        graph,
                        insn.address,
                        insn.operands[1].base,
                        &mut |i, reg| {
                            i.code == ud_mnemonic_code::UD_Imov
                                && i.operands[0].type_ == ud_type::UD_OP_REG
                                && i.operands[0].base == reg
                        },
                    );
                }

                if insn.operands[1].type_ == ud_type::UD_OP_IMM {
                    return Some(insn.operands[1].lvalue);
                }

                // TODO: logger.Warn("Cannot track assignments from 0x{0:x8} operand type {1}", instr.Offset, instr.Operands.[1].Type)
                return None;
            }

            if graph.get_in_value(insn.address) > 1 {
                // TODO: logger.Warn("Cannot track assignments from 0x{0:x8}, the number of incoming links > 1", instr.Offset)
                return None;
            }

            match insn.code {
                ud_mnemonic_code::UD_Imov => {
                    if insn.operands[0].type_ == ud_type::UD_OP_REG
                        && insn.operands[1].type_ == ud_type::UD_OP_REG
                        && insn.operands[0].base == register
                    {
                        return AssignmentTracker::find(
                            graph,
                            insn.address,
                            insn.operands[1].base,
                            try_match,
                        );
                    }
                }
                _ => (),
            }
        }
        None
    }
}