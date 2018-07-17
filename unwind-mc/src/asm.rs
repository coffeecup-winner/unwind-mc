use std::collections::btree_map::Iter;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;

use udis86::*;

use common::Graph;

#[derive(Clone)]
pub enum LinkType {
    Next,
    Branch,
    Call,
    SwitchCaseJump,
}

#[derive(Clone)]
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
    disassembler: Disassembler,
    bytes: &'a [u8],
    first_address: u64,
    first_address_after_code: u64,
    instructions: BTreeMap<u64, Insn>,
    extra_data: HashMap<u64, ExtraData>,
    instruction_links: HashMap<u64, Vec<Link>>,
    reverse_links: HashMap<u64, Vec<Link>>,
    is_reversed: bool,
    edge_predicate: Box<Fn(&Link) -> bool>,
}

pub fn disassemble(bytes: &[u8], pc: u64) -> Result<InstructionGraph, String> {
    let mut disassembler = Disassembler::new(pc);

    let instructions = disassembler.disassemble(bytes, 0, bytes.len());
    let mut instruction_map = BTreeMap::new();

    for instr in instructions.into_iter() {
        instruction_map.insert(instr.address, instr);
    }
    let last_instruction = instruction_map.iter().next_back().unwrap().1;

    let graph = InstructionGraph {
        disassembler: disassembler,
        bytes: bytes,
        first_address: pc,
        first_address_after_code: last_instruction.address + (last_instruction.length as u64),
        instructions: instruction_map,
        extra_data: HashMap::new(),
        instruction_links: HashMap::new(),
        reverse_links: HashMap::new(),
        is_reversed: false,
        edge_predicate: Box::new(|_e| true),
    };

    Ok(graph)
}

impl<'a> Graph<u64, Insn, Link> for InstructionGraph<'a> {
    fn set_subgraph(&mut self, _subgraph: Option<HashSet<u64>>) -> &Self {
        panic!("NOT SUPPORTED");
    }

    fn set_edge_filter(&mut self, filter: Box<Fn(&Link) -> bool>) -> &Self {
        self.edge_predicate = filter;
        self
    }

    fn reverse_edges(&mut self) -> &Self {
        self.is_reversed = !self.is_reversed;
        self
    }

    fn contains(&self, vid: &u64) -> bool {
        self.instructions.contains_key(vid)
    }

    fn get_vertex(&self, vid: &u64) -> &Insn {
        self.instructions.get(vid).unwrap()
    }

    fn get_adjacent(&self, vid: &u64) -> Vec<Result<(&u64, &Link), String>> {
        let mut result = vec![];
        let instruction_links = if self.is_reversed {
            &self.reverse_links
        } else {
            &self.instruction_links
        };
        match instruction_links.get(vid) {
            Some(links) => for link in links.iter().filter(|e| (*self.edge_predicate)(e)) {
                let address = if self.is_reversed {
                    &link.address
                } else {
                    &link.target_address
                };
                if self.in_bounds(*address) {
                    result.push(Result::Ok((address, link)));
                } else {
                    result.push(Result::Err(format!(
                        "DFS: Jump outside of code section: {}",
                        address
                    )));
                }
            },
            None => result.push(Result::Err(format!(
                "DFS: Couldn't find links for {}",
                self.instructions.get(vid).unwrap().to_string()
            ))),
        }
        result
    }
}

impl<'a> InstructionGraph<'a> {
    pub fn instructions_iter(&self) -> Iter<u64, Insn> {
        self.instructions.iter()
    }

    pub fn first_address_after_code(&self) -> u64 {
        self.first_address_after_code
    }

    pub fn contains_address(&self, address: u64) -> bool {
        self.instructions.contains_key(&address)
    }

    pub fn contains(&self, instr: Insn) -> bool {
        false // self.instructions.contains_key(&instr.address())
    }

    pub fn in_bounds(&self, address: u64) -> bool {
        address >= self.first_address && address < self.first_address_after_code
    }

    pub fn get_next(&self, address: u64) -> u64 {
        match (address + 1..self.first_address_after_code - 1)
            .find(|a| self.instructions.contains_key(a))
        {
            Some(address) => address,
            None => panic!("Failed to find the next address."),
        }
    }

    pub fn get_in_value(&self, address: u64) -> u32 {
        self.reverse_links[&address].len() as u32
    }

    pub fn get_out_value(&self, address: u64) -> u32 {
        self.instruction_links[&address].len() as u32
    }

    // pub fn get_bytes(...)

    // pub fn get_extra_data(...)

    pub fn add_link(&mut self, offset: u64, target_offset: u64, type_: LinkType) -> () {
        let link = Link {
            address: offset,
            target_address: target_offset,
            type_,
        };

        let links = match self.instruction_links.get_mut(&offset) {
            Some(links) => links,
            None => {
                let links = Vec::new();
                self.instruction_links.insert(offset, links);
                self.instruction_links.get_mut(&offset).unwrap()
            }
        };
        links.push(link.clone());

        let reverse_links = match self.reverse_links.get_mut(&offset) {
            Some(links) => links,
            None => {
                let links = Vec::new();
                self.instruction_links.insert(offset, links);
                self.instruction_links.get_mut(&offset).unwrap()
            }
        };
        reverse_links.push(link);
    }

    // pub fn add_jump_table_entry(...)

    // pub fn add_jump_table_indirect_entries(...)

    // pub fn split_instruction_at(...)

    // pub fn read_u32(...)

    // pub fn redisassemble(...)

    // fn to_byte_array_index(...)
}
