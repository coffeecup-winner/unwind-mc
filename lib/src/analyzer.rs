use std::collections::{BTreeMap, HashSet};

use asm::*;
use assignment_tracker::*;
use common::*;
use function::*;
use disassembler::*;
use project::*;

pub fn analyze(graph: &mut InstructionGraph, functions: &mut BTreeMap<u64, Function>,
    jump_tables: &mut BTreeMap<u64, JumpTable>) {
    trace!("Analyzer::analyze");
    add_explicit_calls(graph, functions);
    resolve_function_bounds(graph, functions, jump_tables);
    resolve_external_function_calls(graph);
    trace!("Analyzer::analyze: done");
}

fn add_explicit_calls(graph: &mut InstructionGraph, functions: &mut BTreeMap<u64, Function>) -> () {
    let mut calls = vec![];
    for (_, insn) in graph.instructions_iter() {
        if insn.code == Mnemonic::Icall
            && insn.operands[0].is_reladdr()
        {
            calls.push(insn.clone());
        }
    }
    for call in calls {
        let target_address = call.get_target_address();
        graph
            .add_link(call.address, target_address, LinkType::Call);
        functions.entry(target_address).or_insert(Function {
            address: target_address,
            status: FunctionStatus::Created,
            calling_convention: CallingConvention::Unknown,
            arguments_size: None,
        });
    }
}

fn resolve_function_bounds(graph: &mut InstructionGraph, functions: &mut BTreeMap<u64, Function>,
    jump_tables: &mut BTreeMap<u64, JumpTable>) {
    trace!("Analyzer::resolve_function_bounds");
    for func in functions.values_mut() {
        trace!("Analyzer::resolve_function_bounds: 0x{:x}", func.address);
        if !graph.in_bounds(func.address) {
            func.status = FunctionStatus::BoundsNotResolvedInvalidAddress;
            continue;
        }
        if !graph.contains_address(func.address) {
            // The specified address is not a valid start of instruction, re-disassembling
            graph.redisassemble(func.address);
        }

        let mut stack = vec![func.address];
        let mut visited = HashSet::new();
        visited.insert(func.address);
        let mut visited_all_links = true;
        while !stack.is_empty() {
            let address = stack.pop().unwrap();
            graph.get_extra_data_mut(address).function_address = func.address;
            if !graph.contains_address(address) {
                warn!("Analyzer::resolve_function_bounds: WARNING: trying to access address 0x{:x}", address);
                continue;
            }
            let insn = graph.get_vertex(&address).clone();
            if insn.code == Mnemonic::Iret {
                match &insn.operands[..] {
                    &[Operand::ImmediateUnsigned(_, v)] => {
                        func.calling_convention = CallingConvention::Stdcall;
                        func.arguments_size = Some(v as u16);
                    }
                    _ => {}
                }
                continue;
            }

            add_next_links(graph, &insn);
            add_explicit_branches(graph, &insn);
            add_switch_cases(graph, jump_tables, &insn);

            for pair in graph.get_adjacent(&address).into_iter().rev() {
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
    trace!("Analyzer::resolve_function_bounds: done");
}

fn add_next_links(graph: &mut InstructionGraph, insn: &Insn) -> () {
    match insn.code {
        Mnemonic::Iret | Mnemonic::Ijmp | Mnemonic::Iint3 => {
            ()
        }
        _ => {
            let next = graph.get_next(insn.address);
            if next != graph.first_address_after_code() {
                graph.add_link(insn.address, next, LinkType::Next);
            }
        }
    }
}

fn add_explicit_branches(graph: &mut InstructionGraph, insn: &Insn) -> () {
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
            if insn.operands[0].is_reladdr() {
                graph
                    .add_link(insn.address, insn.get_target_address(), LinkType::Branch);
            }
        }
        _ => {}
    }
}

fn add_switch_cases(graph: &mut InstructionGraph, jump_tables: &mut BTreeMap<u64, JumpTable>, insn: &Insn) -> () {
    if insn.code != Mnemonic::Ijmp {
        return;
    }
    if let Operand::MemoryRelative(_, _, _, _, _, v) = insn.operands[0] {
        let address = v as u64;
        if !jump_tables.contains_key(&address) {
            let mut table = JumpTable::new(insn.address, address);
            resolve_jump_table(graph, &mut table);
            jump_tables.insert(address, table);
        }
        let table = jump_tables[&address];
        for i in table.first_index..table.count {
            graph.add_link(
                table.reference,
                u64::from(
                    graph
                        .read_u32(table.address + u64::from(i * REGISTER_SIZE)),
                ),
                LinkType::SwitchCaseJump,
            );
        }
    }
}

fn resolve_jump_table(graph: &mut InstructionGraph, table: &mut JumpTable) -> () {
    if table.address >= graph.first_address_after_code() {
        // Jump table is not in the code segment
        return;
    }

    let mut idx = Reg::NONE;
    let mut low_byte_idx = Reg::NONE;
    let mut indirect_access = 0;

    let cases_count_option = {
        graph
            .set_edge_filter(Box::new(|e: &Link| match e.type_ {
                LinkType::Next | LinkType::Branch => true,
                _ => false,
            })).reverse_edges();
        graph.dfs_pick(&table.reference, &mut |insn, _| {
            if insn.operands.len() < 1 {
                return Pick::Continue;
            }
            match insn.operands[0] {
                Operand::MemoryRelative(_, _, base, index, _, _) => {
                    // find out the jump index register
                    if idx == Reg::NONE {
                        idx = index;
                        low_byte_idx = get_low_byte_register_from_dword(idx);
                        return Pick::Continue;
                    }

                    // update the main jump register if the jump is indirect
                    if indirect_access == 0
                        && insn.code == Mnemonic::Imov
                        && base == low_byte_idx
                    {
                        if let Operand::MemoryRelative(_, _, base, _, _, v) = insn.operands[1] {
                            idx = base;
                            indirect_access = v as u64;
                        } else {
                            panic!("This place should never be reached");
                        }
                        return Pick::Continue;
                    }

                    return Pick::Continue;
                }
                // search for the jump to the default case
                Operand::RelativeAddress(_) if insn.code == Mnemonic::Ija => {
                    // search for cases count, can find it from code like cmp ecx, 0xb
                    // the jump register is irrelevant since it must be the closest one to ja
                    let value = AssignmentTracker::find(&graph, insn.address, idx, &mut |i, _| {
                        if let Operand::Register(_, _) = i.operands[0] {
                            return i.code == Mnemonic::Icmp;
                        }
                        return false;
                    });
                    return Pick::Return(value.map(|v| (v as u8) + 1));
                }
                _ => Pick::Continue,
            }
        })
    };
    // TODO: remove the need for restoring the graph state
    graph
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
                *graph
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
        if !graph.add_jump_table_entry(table.address + offset) {
            table.first_index += 1;
        }
        graph.add_jump_table_entry(table.address + offset);
        offset += u64::from(REGISTER_SIZE);
    }
    table.count = jumps_count;
    while cases_count >= 4 {
        graph
            .add_jump_table_indirect_entries(table.address + offset, 4);
        cases_count -= 4;
        offset += u64::from(REGISTER_SIZE);
    }
    if cases_count > 0 {
        graph
            .add_jump_table_indirect_entries(table.address + offset, cases_count);
        offset += u64::from(cases_count);
    }
    graph.redisassemble(table.address + offset);
}

fn get_low_byte_register_from_dword(reg: Reg) -> Reg {
    match reg {
        Reg::EAX => Reg::AL,
        Reg::EBX => Reg::BL,
        Reg::ECX => Reg::CL,
        Reg::EDX => Reg::DL,
        _ => Reg::NONE,
    }
}

fn resolve_external_function_calls(_graph: &mut InstructionGraph) -> () {
    // TODO
}
