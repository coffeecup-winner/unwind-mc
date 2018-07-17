use std::collections::BTreeMap;

use asm::disassemble;
use asm::InstructionGraph;

pub enum FunctionStatus {
    Created,
    BoundsResolved,
    BoundsNotResolvedInvalidAddress,
    BoundsNotResolvedIncompleteGraph,
}

#[allow(dead_code)]
pub struct Function {
    address: u64,
    status: FunctionStatus,
}

#[allow(dead_code)]
pub struct JumpTable {
    reference: u64,
    address: u64,
    first_index: i32,
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

#[allow(dead_code)]
pub struct Analyzer<'a> {
    graph: InstructionGraph<'a>,
    // import_resolver: ...,
    functions: BTreeMap<u64, Function>,
    jump_tables: BTreeMap<u64, JumpTable>,
}

impl<'a> Analyzer<'a> {
    pub fn create(text_bytes: &'a [u8], pc: u64) -> Result<Analyzer, String> {
        let analyzer = Analyzer {
            graph: disassemble(text_bytes, pc)?,
            functions: BTreeMap::new(),
            jump_tables: BTreeMap::new(),
        };
        Ok(analyzer)
    }

    pub fn graph(&self) -> &InstructionGraph<'a> {
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

    fn add_explicit_calls(&self) -> () {
        for (_, instr) in self.graph.instructions_iter() {
            // TODO
        }
        // TODO
    }

    fn resolve_function_bounds(&mut self) -> () {
        for func in self.functions.values_mut() {
            if !self.graph.in_bounds(func.address) {
                func.status = FunctionStatus::BoundsNotResolvedInvalidAddress;
                continue;
            }
            if !self.graph.contains_address(func.address) {
                // TODO: redisassemble
            }
            let visited_all_links = true; // TODO: do DFS on graph
            func.status = if visited_all_links {
                FunctionStatus::BoundsResolved
            } else {
                FunctionStatus::BoundsNotResolvedIncompleteGraph
            }
        }
    }

    fn resolve_external_function_calls(&self) -> () {
        // TODO
    }
}
