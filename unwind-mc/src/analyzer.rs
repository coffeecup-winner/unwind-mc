use capstone::prelude::*;
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
    pub fn create(text_bytes: &'a [u8], pc: u64) -> CsResult<Analyzer> {
        let analyzer = Analyzer {
            graph: disassemble(text_bytes, pc)?,
            functions: BTreeMap::new(),
            jump_tables: BTreeMap::new(),
        };
        Ok(analyzer)
    }
}
