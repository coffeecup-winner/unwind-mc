use std::collections::BTreeMap;
use std::collections::btree_map::Iter;
use std::fs::File;
use std::io::{Read, Write};
use std::path::PathBuf;

use bincode::{serialize, deserialize};
use goblin::Object;

use analyzer;
use asm::*;
use common::*;
use function::*;

#[derive(Serialize, Deserialize, Debug, Copy, Clone)]
pub struct JumpTable {
    pub reference: u64,
    pub address: u64,
    pub first_index: u32,
    pub count: u32,
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

#[derive(Serialize, Deserialize)]
pub struct Project {
    graph: InstructionGraph,
    // import_resolver: ...,
    functions: BTreeMap<u64, Function>,
    jump_tables: BTreeMap<u64, JumpTable>,
}

impl Project {
    pub fn from_text(text_bytes: Vec<u8>, image_base: u64) -> UResult<Project> {
        Ok(Project {
            graph: disassemble(text_bytes, image_base)?,
            functions: BTreeMap::new(),
            jump_tables: BTreeMap::new(),
        })
    }

    pub fn from_binary_file(path: &str) -> UResult<Project> {
        let path = PathBuf::from(path);
        let mut file = File::open(&path).map_err(|e| e.to_string())?;
        let mut buf = vec![];
        file.read_to_end(&mut buf).map_err(|e| e.to_string())?;
        match Object::parse(&buf).map_err(|e| e.to_string())? {
            Object::Elf(elf) => {
                for section in elf.section_headers {
                    if section.is_executable() {
                        let range = section.file_range();
                        let vm_range = section.vm_range();
                        let text = Vec::from(&buf[range]);
                        return Self::from_text(text, vm_range.start as u64);
                    }
                }
                return Err("ELF has no text section".to_string());
            }
            Object::PE(pe) => {
                for section in pe.sections {
                    match section.name() {
                        Ok(name) if name == ".text" => {
                            let range = section.pointer_to_raw_data as usize ..
                                (section.pointer_to_raw_data as usize + section.size_of_raw_data as usize);
                            let text = Vec::from(&buf[range]);
                            let image_base = pe.image_base as u64 + section.virtual_address as u64;
                            let mut project = Self::from_text(text, image_base)?;
                            let entry_point_address =
                                pe.image_base as u64 + pe.header.optional_header.unwrap().standard_fields.address_of_entry_point as u64;
                            project.add_function(entry_point_address);
                            return Ok(project);
                        }
                        _ => {}
                    }
                }
                return Err("PE has no text section".to_string());
            }
            _ => return Err("Invalid file format".to_string()),
        }
    }

    pub fn from_serialized_file(path: &str) -> UResult<Project> {
        let path = PathBuf::from(path);
        let mut file = File::open(&path).map_err(|e| e.to_string())?;
        let mut buf = vec![];
        file.read_to_end(&mut buf).expect("Failed to read file");
        deserialize(&buf[..]).map_err(|e| e.to_string())
    }

    pub fn serialize_to_file(&self, path: &str) -> UResult<()> {
        let path = PathBuf::from(path);
        let mut file = File::create(&path).map_err(|e| e.to_string())?;
        let buf = serialize(self).map_err(|e| e.to_string())?;
        file.write_all(&buf[..]).map_err(|e| e.to_string())?;
        Ok(())
    }

    pub fn graph(&self) -> &InstructionGraph {
        &self.graph
    }

    pub fn functions_iter(&self) -> Iter<u64, Function> {
        self.functions.iter()
    }

    pub fn add_function(&mut self, address: u64) -> () {
        trace!("Project::add_function: 0x{:x}", address);
        self.functions.insert(
            address,
            Function {
                address,
                status: FunctionStatus::Created,
                calling_convention: CallingConvention::Unknown,
                arguments_size: None,
            },
        );
    }

    pub fn analyze_asm(&mut self) {
        analyzer::analyze(&mut self.graph, &mut self.functions, &mut self.jump_tables);
    }
}
