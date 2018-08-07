use std;
use std::collections::btree_map::Iter;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;

use libudis86_sys::{ud_mnemonic_code, ud_type};

use common::Graph;
use udis86::*;

#[derive(Debug, Copy, Clone)]
pub enum LinkType {
    Next,
    Branch,
    Call,
    SwitchCaseJump,
}

#[derive(Debug, Copy, Clone)]
pub struct Link {
    pub address: u64,
    pub target_address: u64,
    pub type_: LinkType,
}

#[derive(Debug, Clone)]
pub struct ExtraData {
    pub function_address: u64,
    pub import_name: String,
    pub is_protected: bool,
}

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
    fn set_subgraph(&mut self, _subgraph: Option<HashSet<u64>>) -> &mut Self {
        panic!("NOT SUPPORTED");
    }

    fn set_edge_filter(&mut self, filter: Box<Fn(&Link) -> bool>) -> &mut Self {
        self.edge_predicate = filter;
        self
    }

    fn reverse_edges(&mut self) -> &mut Self {
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
        self.instructions.contains_key(&instr.address)
    }

    pub fn in_bounds(&self, address: u64) -> bool {
        address >= self.first_address && address < self.first_address_after_code
    }

    pub fn get_next(&self, address: u64) -> u64 {
        match (address + 1..self.first_address_after_code)
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

    pub fn get_bytes(&self, address: u64, size: usize) -> &[u8] {
        let addr = self.to_byte_array_index(address);
        &self.bytes[addr..(addr + size)]
    }

    pub fn get_extra_data(&mut self, address: u64) -> &mut ExtraData {
        if !self.extra_data.contains_key(&address) {
            let data = ExtraData {
                function_address: 0,
                import_name: "".to_string(),
                is_protected: false,
            };
            self.extra_data.insert(address, data);
        }
        self.extra_data.get_mut(&address).unwrap()
    }

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

        let reverse_links = match self.reverse_links.get_mut(&target_offset) {
            Some(links) => links,
            None => {
                let links = Vec::new();
                self.reverse_links.insert(target_offset, links);
                self.reverse_links.get_mut(&target_offset).unwrap()
            }
        };
        reverse_links.push(link);
    }

    pub fn add_jump_table_entry(&mut self, address: u64) -> bool {
        let data = self.read_u32(address);
        if !self.in_bounds(data as u64) {
            false
        } else {
            self.mark_data_bytes(address, 4, "TODO: TEXT".to_string());
            true
        }
    }

    pub fn add_jump_table_indirect_entries(&mut self, address: u64, count: u8) -> () {
        let display_text = "TODO: TEXT".to_string();
        self.mark_data_bytes(address, count, display_text);
    }

    pub fn mark_data_bytes(&mut self, address: u64, length: u8, data_display_text: String) -> () {
        // check if there is an instruction at address, otherwise split an existing one
        let mut size = match self.instructions.get(&address) {
            Some(instr) => instr.length,
            None => {
                let insn = self.split_instruction_at(address);
                insn.length - ((address - insn.address) as u8)
            }
        };
        // write a pseudo instruction
        let mut hex = String::new();
        for i in 0..length {
            hex.push_str(&format!(
                "{:02x}",
                self.bytes[self.to_byte_array_index(address + i as u64)]
            ));
        }
        self.instructions.insert(
            address,
            Insn {
                address: address,
                code: ud_mnemonic_code::UD_Inone,
                length: length,
                hex: hex,
                assembly: data_display_text,
                operands: vec![],
                prefix_rex: 0,
                prefix_segment: ud_type::UD_NONE,
                prefix_operand_size: false,
                prefix_address_size: false,
                prefix_lock: 0,
                prefix_str: 0,
                prefix_rep: 0,
                prefix_repe: 0,
                prefix_repne: 0,
            },
        );
        self.get_extra_data(address).is_protected = true;
        // remove old instructions
        while size < length {
            let insn = self
                .instructions
                .remove(&(address + (size as u64)))
                .unwrap();
            size += insn.length;
        }
        // if there are bytes left from the last removed instruction, re-disassemble them
        if size != length {
            self.disassembler.set_pc(address + (length as u64));
            let new_instructions = self.disassembler.disassemble(
                self.bytes,
                self.to_byte_array_index(address) + (length as usize),
                (size - length) as usize,
            );
            for new_insn in new_instructions {
                self.instructions.insert(new_insn.address, new_insn);
            }
        }
    }

    pub fn split_instruction_at(&mut self, address: u64) -> Insn {
        let mut insn_address = address - 1;
        while !self.instructions.contains_key(&insn_address) {
            insn_address -= 1;
        }
        let old_insn = self.instructions.get(&insn_address).unwrap().clone();
        // Split the old instruction and re-disassemble its first part
        let extra_length = (address - insn_address) as usize;
        self.disassembler.set_pc(insn_address);
        let new_instructions = self.disassembler.disassemble(
            self.bytes,
            self.to_byte_array_index(insn_address),
            extra_length,
        );
        for new_insn in new_instructions {
            self.instructions.insert(new_insn.address, new_insn);
        }
        old_insn
    }

    pub fn read_u32(&self, address: u64) -> u32 {
        let addr = (address - self.first_address) as usize;
        let mut result = self.bytes[addr] as u32;
        result |= (self.bytes[addr + 1] as u32) << 8;
        result |= (self.bytes[addr + 2] as u32) << 16;
        result |= (self.bytes[addr + 3] as u32) << 24;
        result
    }

    pub fn redisassemble(&mut self, address: u64) -> () {
        // This function will fix any instructions that were incorrectly disassembled because of the data block that was treated as code
        if !self.instructions.contains_key(&address) {
            self.split_instruction_at(address);
        }
        self.disassembler.set_pc(address);
        let max_disasm_length = 0x100;
        let mut disasm_length =
            std::cmp::min(max_disasm_length, self.first_address_after_code - address);
        for j in 0..(disasm_length + 1) {
            match self.extra_data.get(&(address + j)) {
                Some(data) if data.is_protected => {
                    disasm_length = j;
                }
                _ => (),
            }
        }
        let mut new_instructions = self.disassembler.disassemble(
            self.bytes,
            self.to_byte_array_index(address),
            disasm_length as usize,
        );
        // Take 1 instruction less since it can be incorrectly disassembled (partial data)
        new_instructions.pop();
        let mut fixed_instructions = false;
        for new_insn in new_instructions {
            match self.instructions.get(&new_insn.address) {
                Some(old_insn) if old_insn.length == new_insn.length => {
                    if fixed_instructions {
                        return;
                    }
                }
                _ => {
                    let length = new_insn.length as u64;
                    let addr = new_insn.address;
                    self.instructions.insert(new_insn.address, new_insn);
                    for j in 1..length {
                        self.instructions.remove(&(addr + j));
                    }
                    fixed_instructions = true;
                }
            }
        }
        panic!("FIXME: Extra disassemble size was too small");
    }

    fn to_byte_array_index(&self, address: u64) -> usize {
        (address - self.first_address) as usize
    }
}
