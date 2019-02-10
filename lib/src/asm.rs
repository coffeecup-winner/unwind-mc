use std;
use std::collections::btree_map::Iter;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;

use common::Graph;
use disassembler::*;

#[derive(Serialize, Deserialize, Debug, Copy, Clone)]
pub enum LinkType {
    Next,
    Branch,
    Call,
    SwitchCaseJump,
}

#[derive(Serialize, Deserialize, Debug, Copy, Clone)]
pub struct Link {
    pub address: u64,
    pub target_address: u64,
    pub type_: LinkType,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ExtraData {
    pub function_address: u64,
    pub import_name: String,
    pub is_protected: bool,
}

#[derive(Serialize, Deserialize)]
pub struct InstructionGraph {
    disassembler: Disassembler,
    bytes: Vec<u8>,
    first_address: u64,
    first_address_after_code: u64,
    instructions: BTreeMap<u64, Insn>,
    extra_data: HashMap<u64, ExtraData>,
    instruction_links: HashMap<u64, Vec<Link>>,
    reverse_links: HashMap<u64, Vec<Link>>,
    is_reversed: bool,
    #[serde(skip, default = "empty_edge_predicate")]
    edge_predicate: Box<Fn(&Link) -> bool>,
}

fn empty_edge_predicate() -> Box<Fn(&Link) -> bool> {
    Box::new(|_e| true)
}

pub fn disassemble(bytes: Vec<u8>, pc: u64) -> Result<InstructionGraph, String> {
    trace!("InstructionGraph::disassemble: 0x{:x}, {} bytes", pc, bytes.len());
    let mut disassembler = Disassembler::new();

    let instructions = disassembler.disassemble(&bytes[..], pc, 0, bytes.len());
    let mut instruction_map = BTreeMap::new();

    for instr in instructions.into_iter() {
        instruction_map.insert(instr.address, instr);
    }
    let last_instruction = instruction_map.iter().next_back().unwrap().1;

    let graph = InstructionGraph {
        disassembler,
        bytes,
        first_address: pc,
        first_address_after_code: last_instruction.address + u64::from(last_instruction.length),
        instructions: instruction_map,
        extra_data: HashMap::new(),
        instruction_links: HashMap::new(),
        reverse_links: HashMap::new(),
        is_reversed: false,
        edge_predicate: empty_edge_predicate(),
    };

    trace!("InstructionGraph::disassemble: done");
    Ok(graph)
}

impl Graph<u64, Insn, Link> for InstructionGraph {
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
        &self.instructions[vid]
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
                &self.instructions[vid].to_string()
            ))),
        }
        result
    }
}

impl InstructionGraph {
    pub fn instructions_iter(&self) -> Iter<u64, Insn> {
        self.instructions.iter()
    }

    pub fn first_address_after_code(&self) -> u64 {
        self.first_address_after_code
    }

    pub fn contains_address(&self, address: u64) -> bool {
        self.instructions.contains_key(&address)
    }

    pub fn contains(&self, instr: &Insn) -> bool {
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

    pub fn get_extra_data(&self, address: u64) -> Option<&ExtraData> {
        self.extra_data.get(&address)
    }

    pub fn get_extra_data_mut(&mut self, address: u64) -> &mut ExtraData {
        self.extra_data.entry(address).or_insert(ExtraData {
            function_address: 0,
            import_name: "".to_string(),
            is_protected: false,
        })
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
        links.push(link);

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
        if !self.in_bounds(u64::from(data)) {
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
                self.bytes[self.to_byte_array_index(address + u64::from(i))]
            ));
        }
        self.instructions.insert(
            address,
            Insn {
                address,
                code: Mnemonic::Inone,
                length,
                hex,
                assembly: data_display_text,
                operands: vec![],
                prefix_rex: 0,
                prefix_segment: Reg::NONE,
                prefix_operand_size: false,
                prefix_address_size: false,
                prefix_lock: 0,
                prefix_str: 0,
                prefix_rep: 0,
                prefix_repe: 0,
                prefix_repne: 0,
            },
        );
        self.get_extra_data_mut(address).is_protected = true;
        // remove old instructions
        while size < length {
            let insn = self
                .instructions
                .remove(&(address + u64::from(size)))
                .unwrap();
            size += insn.length;
        }
        // if there are bytes left from the last removed instruction, re-disassemble them
        if size != length {
            let new_instructions = self.disassembler.disassemble(
                &self.bytes[..],
                address + u64::from(length),
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
        let old_insn = &self.instructions[&insn_address].clone();
        // Split the old instruction and re-disassemble its first part
        let extra_length = (address - insn_address) as usize;
        let new_instructions = self.disassembler.disassemble(
            &self.bytes[..],
            insn_address,
            self.to_byte_array_index(insn_address),
            extra_length,
        );
        for new_insn in new_instructions {
            self.instructions.insert(new_insn.address, new_insn);
        }
        old_insn.clone()
    }

    pub fn read_u32(&self, address: u64) -> u32 {
        let addr = (address - self.first_address) as usize;
        let mut result = u32::from(self.bytes[addr]);
        result |= u32::from(self.bytes[addr + 1]) << 8;
        result |= u32::from(self.bytes[addr + 2]) << 16;
        result |= u32::from(self.bytes[addr + 3]) << 24;
        result
    }

    pub fn redisassemble(&mut self, address: u64) -> () {
        // This function will fix any instructions that were incorrectly disassembled because of the data block that was treated as code
        trace!("InstructionGraph::redisassemble: 0x{:x}", address);
        if !self.instructions.contains_key(&address) {
            self.split_instruction_at(address);
        }
        let max_disasm_length = 0x100;
        let mut disasm_length =
            std::cmp::min(max_disasm_length, self.first_address_after_code - address);
        disasm_length = (0..=disasm_length)
            .find(|len| {
                self.extra_data
                    .get(&(address + len))
                    .map_or(false, |d| d.is_protected)
            }).unwrap_or(disasm_length);
        let mut new_instructions = self.disassembler.disassemble(
            &self.bytes[..],
            address,
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
                        trace!("InstructionGraph::redisassemble: done");
                        return;
                    }
                }
                _ => {
                    let length = u64::from(new_insn.length);
                    let addr = new_insn.address;
                    self.instructions.insert(new_insn.address, new_insn);
                    for j in 1..length {
                        self.instructions.remove(&(addr + j));
                    }
                    fixed_instructions = true;
                }
            }
        }
        error!("InstructionGraph::redisassemble: FIXME: Extra disassemble size was too small");
    }

    fn to_byte_array_index(&self, address: u64) -> usize {
        (address - self.first_address) as usize
    }
}
