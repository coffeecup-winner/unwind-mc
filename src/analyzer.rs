use std::collections::{BTreeMap, HashSet};

use libudis86_sys::{ud_mnemonic_code, ud_type};

use asm::*;
use assignment_tracker::*;
use common::*;
use udis86::*;

#[derive(Debug, Copy, Clone)]
pub enum FunctionStatus {
    Created,
    BoundsResolved,
    BoundsNotResolvedInvalidAddress,
    BoundsNotResolvedIncompleteGraph,
}

#[derive(Debug, Copy, Clone)]
pub struct Function {
    address: u64,
    status: FunctionStatus,
}

#[derive(Debug, Copy, Clone)]
pub struct JumpTable {
    reference: u64,
    address: u64,
    first_index: u32,
    count: u32,
}

impl JumpTable {
    pub fn new(reference: u64, address: u64) -> JumpTable {
        JumpTable {
            reference,
            address,
            first_index: 0,
            count: 0,
        }
    }
}

pub struct Analyzer {
    graph: InstructionGraph,
    // import_resolver: ...,
    functions: BTreeMap<u64, Function>,
    jump_tables: BTreeMap<u64, JumpTable>,
}

impl Analyzer {
    pub fn create(text_bytes: Vec<u8>, pc: u64) -> Result<Analyzer, String> {
        let analyzer = Analyzer {
            graph: disassemble(text_bytes, pc)?,
            functions: BTreeMap::new(),
            jump_tables: BTreeMap::new(),
        };
        Ok(analyzer)
    }

    pub fn graph(&self) -> &InstructionGraph {
        &self.graph
    }

    pub fn add_function(&mut self, address: u64) -> () {
        self.functions.insert(
            address,
            Function {
                address,
                status: FunctionStatus::Created,
            },
        );
    }

    pub fn analyze(&mut self) -> () {
        self.add_explicit_calls();
        self.resolve_function_bounds();
        self.resolve_external_function_calls();
    }

    fn add_explicit_calls(&mut self) -> () {
        let mut calls = vec![];
        for (_, insn) in self.graph.instructions_iter() {
            if insn.code == ud_mnemonic_code::UD_Icall
                && insn.operands[0].type_ == ud_type::UD_OP_JIMM
            {
                calls.push(insn.clone());
            }
        }
        for call in calls {
            let target_address = call.get_target_address();
            self.graph
                .add_link(call.address, target_address, LinkType::Call);
            if !self.functions.contains_key(&target_address) {
                self.functions.insert(
                    target_address,
                    Function {
                        address: target_address,
                        status: FunctionStatus::Created,
                    },
                );
            }
        }
    }

    fn resolve_function_bounds(&mut self) -> () {
        // TODO: remove copying
        let mut functions = self.functions.clone();
        for func in functions.values_mut() {
            if !self.graph.in_bounds(func.address) {
                func.status = FunctionStatus::BoundsNotResolvedInvalidAddress;
                continue;
            }
            if !self.graph.contains_address(func.address) {
                // The specified address is not a valid start of instruction, re-disassembling
                self.graph.redisassemble(func.address);
            }

            let mut stack = vec![func.address];
            let mut visited = HashSet::new();
            visited.insert(func.address);
            let mut visited_all_links = true;
            while stack.len() > 0 {
                let address = stack.pop().unwrap();
                self.graph.get_extra_data(address).function_address = func.address;
                let insn = self.graph.get_vertex(&address).clone();
                if insn.code == ud_mnemonic_code::UD_Iret {
                    continue;
                }

                self.add_next_links(&insn);
                self.add_explicit_branches(&insn);
                self.add_switch_cases(&insn);

                for pair in self.graph.get_adjacent(&address).into_iter().rev() {
                    if pair.is_err() {
                        visited_all_links = false;
                    } else {
                        let (addr, link) = pair.unwrap();
                        match link.type_ {
                            LinkType::Next | LinkType::Branch | LinkType::SwitchCaseJump => {
                                if !visited.contains(addr) {
                                    stack.push(*addr);
                                    visited.insert(*addr);
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
            func.status = if visited_all_links {
                FunctionStatus::BoundsResolved
            } else {
                FunctionStatus::BoundsNotResolvedIncompleteGraph
            }
        }
        self.functions = functions;
    }

    fn add_next_links(&mut self, insn: &Insn) -> () {
        match insn.code {
            ud_mnemonic_code::UD_Iret | ud_mnemonic_code::UD_Ijmp | ud_mnemonic_code::UD_Iint3 => {
                ()
            }
            _ => {
                let next = self.graph.get_next(insn.address);
                if next != self.graph.first_address_after_code() {
                    self.graph.add_link(insn.address, next, LinkType::Next);
                }
            }
        }
    }

    fn add_explicit_branches(&mut self, insn: &Insn) -> () {
        match insn.code {
            ud_mnemonic_code::UD_Ija
            | ud_mnemonic_code::UD_Ijae
            | ud_mnemonic_code::UD_Ijb
            | ud_mnemonic_code::UD_Ijbe
            | ud_mnemonic_code::UD_Ijcxz
            | ud_mnemonic_code::UD_Ijecxz
            | ud_mnemonic_code::UD_Ijg
            | ud_mnemonic_code::UD_Ijge
            | ud_mnemonic_code::UD_Ijl
            | ud_mnemonic_code::UD_Ijle
            | ud_mnemonic_code::UD_Ijmp
            | ud_mnemonic_code::UD_Ijno
            | ud_mnemonic_code::UD_Ijnp
            | ud_mnemonic_code::UD_Ijns
            | ud_mnemonic_code::UD_Ijnz
            | ud_mnemonic_code::UD_Ijo
            | ud_mnemonic_code::UD_Ijp
            | ud_mnemonic_code::UD_Ijrcxz
            | ud_mnemonic_code::UD_Ijs
            | ud_mnemonic_code::UD_Ijz => {
                if insn.operands[0].type_ == ud_type::UD_OP_JIMM {
                    self.graph
                        .add_link(insn.address, insn.get_target_address(), LinkType::Branch);
                }
            }
            _ => {}
        }
    }

    fn add_switch_cases(&mut self, insn: &Insn) -> () {
        if insn.code != ud_mnemonic_code::UD_Ijmp || insn.operands[0].type_ != ud_type::UD_OP_MEM {
            return;
        }

        // TODO: move unsafe into udis86 module
        let address = unsafe { insn.operands[0].lvalue.udword } as u64;
        if !self.jump_tables.contains_key(&address) {
            let mut table = JumpTable::new(insn.address, address);
            self.resolve_jump_table(&mut table);
            self.jump_tables.insert(address, table);
        }
        let table = self.jump_tables.get(&address).unwrap();
        for i in table.first_index..table.count {
            self.graph.add_link(
                table.reference,
                self.graph
                    .read_u32(table.address + (i * REGISTER_SIZE) as u64) as u64,
                LinkType::SwitchCaseJump,
            );
        }
    }

    fn resolve_jump_table(&mut self, table: &mut JumpTable) -> () {
        if table.address >= self.graph.first_address_after_code() {
            // Jump table is not in the code segment
            return;
        }

        let mut idx = ud_type::UD_NONE;
        let mut low_byte_idx = ud_type::UD_NONE;
        let mut indirect_access = 0;

        let cases_count_option = {
            self.graph
                .set_edge_filter(Box::new(|e: &Link| match e.type_ {
                    LinkType::Next | LinkType::Branch => true,
                    _ => false,
                })).reverse_edges();
            self.graph.dfs_pick(&table.reference, &mut |insn, _| {
                // find out the jump index register
                if idx == ud_type::UD_NONE {
                    idx = insn.operands[0].index;
                    low_byte_idx = Analyzer::get_low_byte_register_from_dword(idx);
                    return Pick::Continue;
                }

                // update the main jump register if the jump is indirect
                if indirect_access == 0
                    && insn.code == ud_mnemonic_code::UD_Imov
                    && insn.operands[0].base == low_byte_idx
                {
                    idx = insn.operands[1].base;
                    indirect_access = unsafe { insn.operands[1].lvalue.udword } as u64;
                    return Pick::Continue;
                }

                // search for the jump to the default case
                if insn.code != ud_mnemonic_code::UD_Ija
                    || insn.operands[0].type_ != ud_type::UD_OP_JIMM
                {
                    return Pick::Continue;
                }

                // search for cases count, can find it from code like cmp ecx, 0xb
                // the jump register is irrelevant since it must be the closest one to ja
                let value = AssignmentTracker::find(&self.graph, insn.address, idx, &mut |i, _| {
                    i.code == ud_mnemonic_code::UD_Icmp && i.operands[0].type_ == ud_type::UD_OP_REG
                });
                Pick::Return(value.map(|v| unsafe { v.ubyte } + 1))
            })
        };
        // TODO: remove the need for restoring the graph state
        self.graph
            .set_edge_filter(Box::new(|_| true))
            .reverse_edges();

        if cases_count_option.is_none() {
            return;
        }

        let cases_count = cases_count_option.unwrap();
        let (jumps_count, mut cases_count) = if indirect_access == 0 {
            (cases_count as u32, 0)
        } else {
            (
                *self
                    .graph
                    .get_bytes(indirect_access, cases_count as usize)
                    .iter()
                    .max()
                    .unwrap() as u32
                    + 1,
                cases_count,
            )
        };

        let mut offset = 0;
        for _ in 0..jumps_count {
            if !self.graph.add_jump_table_entry(table.address + offset) {
                table.first_index += 1;
            }
            self.graph.add_jump_table_entry(table.address + offset);
            offset += REGISTER_SIZE as u64;
        }
        table.count = jumps_count;
        while cases_count >= 4 {
            self.graph
                .add_jump_table_indirect_entries(table.address + offset, 4);
            cases_count -= 4;
            offset += REGISTER_SIZE as u64;
        }
        if cases_count > 0 {
            self.graph
                .add_jump_table_indirect_entries(table.address + offset, cases_count);
            offset += cases_count as u64;
        }
        self.graph.redisassemble(table.address + offset);
    }

    fn get_low_byte_register_from_dword(reg: ud_type) -> ud_type {
        match reg {
            ud_type::UD_R_EAX => ud_type::UD_R_AL,
            ud_type::UD_R_EBX => ud_type::UD_R_BL,
            ud_type::UD_R_ECX => ud_type::UD_R_CL,
            ud_type::UD_R_EDX => ud_type::UD_R_DL,
            _ => ud_type::UD_NONE,
        }
    }

    fn resolve_external_function_calls(&self) -> () {
        // TODO
    }
}
