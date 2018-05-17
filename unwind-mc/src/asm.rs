use capstone::prelude::*;
use capstone::*;
use std::collections::BTreeMap;
use std::collections::HashMap;

pub enum LinkType {
    Next,
    Branch,
    Call,
    SwitchCaseJump,
}

pub struct Link {
    pub address: u64,
    pub target_address: u64,
    pub type_: LinkType,
}

pub struct ExtraData {
    pub function_address: u64,
    pub import_name: String,
    pub is_protected: bool,
}

#[allow(dead_code)]
pub struct InstructionGraph<'a> {
    disassembler: Capstone,
    bytes: &'a [u8],
    first_address: u64,
    first_address_after_code: u64,
    instructions: BTreeMap<u64, Insn>,
    extra_data: HashMap<u64, ExtraData>,
    instruction_links: HashMap<u64, Vec<Link>>,
    reverse_links: HashMap<u64, Vec<Link>>,
    is_reversed: bool,
    edge_predicate: Box<Fn(Link) -> bool>,
}

#[allow(dead_code)]
fn disassemble(bytes: &[u8], pc: u64) -> CsResult<InstructionGraph> {
    let cs = Capstone::new()
        .x86()
        .mode(arch::x86::ArchMode::Mode32)
        .syntax(arch::x86::ArchSyntax::Intel)
        .detail(true)
        .build()?;

    let instructions = cs.disasm_all(bytes, pc)?;
    let mut instruction_map = BTreeMap::new();

    for instr in instructions.iter() {
        instruction_map.insert(instr.address(), instr);
    }
    let last_instruction = instruction_map.iter().next_back().unwrap().1;

    let graph = InstructionGraph {
        disassembler: cs,
        bytes: bytes,
        first_address: pc,
        first_address_after_code: last_instruction.address()
            + (last_instruction.bytes().len() as u64),
        instructions: instruction_map,
        extra_data: HashMap::new(),
        instruction_links: HashMap::new(),
        reverse_links: HashMap::new(),
        is_reversed: false,
        edge_predicate: Box::new(|_e| true),
    };

    Ok(graph)
}
