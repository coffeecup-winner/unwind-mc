use std::collections::{BTreeMap, HashSet};

use libudis86_sys::{ud_type};

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
            if insn.code == Mnemonic::Icall
                && insn.operands[0].type_ == ud_type::UD_OP_JIMM
            {
                calls.push(insn.clone());
            }
        }
        for call in calls {
            let target_address = call.get_target_address();
            self.graph
                .add_link(call.address, target_address, LinkType::Call);
            self.functions.entry(target_address).or_insert(Function {
                address: target_address,
                status: FunctionStatus::Created,
            });
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
            while !stack.is_empty() {
                let address = stack.pop().unwrap();
                self.graph.get_extra_data(address).function_address = func.address;
                let insn = self.graph.get_vertex(&address).clone();
                if insn.code == Mnemonic::Iret {
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
            Mnemonic::Iret | Mnemonic::Ijmp | Mnemonic::Iint3 => {
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
            Mnemonic::Ija
            | Mnemonic::Ijae
            | Mnemonic::Ijb
            | Mnemonic::Ijbe
            | Mnemonic::Ijcxz
            | Mnemonic::Ijecxz
            | Mnemonic::Ijg
            | Mnemonic::Ijge
            | Mnemonic::Ijl
            | Mnemonic::Ijle
            | Mnemonic::Ijmp
            | Mnemonic::Ijno
            | Mnemonic::Ijnp
            | Mnemonic::Ijns
            | Mnemonic::Ijnz
            | Mnemonic::Ijo
            | Mnemonic::Ijp
            | Mnemonic::Ijrcxz
            | Mnemonic::Ijs
            | Mnemonic::Ijz => {
                if insn.operands[0].type_ == ud_type::UD_OP_JIMM {
                    self.graph
                        .add_link(insn.address, insn.get_target_address(), LinkType::Branch);
                }
            }
            _ => {}
        }
    }

    fn add_switch_cases(&mut self, insn: &Insn) -> () {
        if insn.code != Mnemonic::Ijmp || insn.operands[0].type_ != ud_type::UD_OP_MEM {
            return;
        }

        // TODO: move unsafe into udis86 module
        let address = u64::from(unsafe { insn.operands[0].lvalue.udword });
        if !self.jump_tables.contains_key(&address) {
            let mut table = JumpTable::new(insn.address, address);
            self.resolve_jump_table(&mut table);
            self.jump_tables.insert(address, table);
        }
        let table = self.jump_tables[&address];
        for i in table.first_index..table.count {
            self.graph.add_link(
                table.reference,
                u64::from(
                    self.graph
                        .read_u32(table.address + u64::from(i * REGISTER_SIZE)),
                ),
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
                    && insn.code == Mnemonic::Imov
                    && insn.operands[0].base == low_byte_idx
                {
                    idx = insn.operands[1].base;
                    indirect_access = u64::from(unsafe { insn.operands[1].lvalue.udword });
                    return Pick::Continue;
                }

                // search for the jump to the default case
                if insn.code != Mnemonic::Ija
                    || insn.operands[0].type_ != ud_type::UD_OP_JIMM
                {
                    return Pick::Continue;
                }

                // search for cases count, can find it from code like cmp ecx, 0xb
                // the jump register is irrelevant since it must be the closest one to ja
                let value = AssignmentTracker::find(&self.graph, insn.address, idx, &mut |i, _| {
                    i.code == Mnemonic::Icmp && i.operands[0].type_ == ud_type::UD_OP_REG
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
            (u32::from(cases_count), 0)
        } else {
            (
                u32::from(
                    *self
                        .graph
                        .get_bytes(indirect_access, cases_count as usize)
                        .iter()
                        .max()
                        .unwrap(),
                ) + 1,
                cases_count,
            )
        };

        let mut offset = 0;
        for _ in 0..jumps_count {
            if !self.graph.add_jump_table_entry(table.address + offset) {
                table.first_index += 1;
            }
            self.graph.add_jump_table_entry(table.address + offset);
            offset += u64::from(REGISTER_SIZE);
        }
        table.count = jumps_count;
        while cases_count >= 4 {
            self.graph
                .add_jump_table_indirect_entries(table.address + offset, 4);
            cases_count -= 4;
            offset += u64::from(REGISTER_SIZE);
        }
        if cases_count > 0 {
            self.graph
                .add_jump_table_indirect_entries(table.address + offset, cases_count);
            offset += u64::from(cases_count);
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
