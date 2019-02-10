use std::fs::File;
use std::io::{Read, Write};
use std::path::PathBuf;

use bincode::{serialize, deserialize};
use goblin::Object;

use analyzer::Analyzer;
use asm::InstructionGraph;
use ast_builder::AstBuilder;
use cpp_emitter::CppEmitter;
use flow_analyzer::build_flow_graph;
use il_decompiler::decompile;
use type_resolver::TypeResolver;

pub fn open_binary_file(path: &str) -> Result<Analyzer, String> {
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
                    return Analyzer::create(text, vm_range.start as u64);
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
                        let mut analyzer = Analyzer::create(text, pe.image_base as u64 + section.virtual_address as u64)?;
                        analyzer.add_function(pe.image_base as u64 + pe.header.optional_header.unwrap().standard_fields.address_of_entry_point as u64);
                        return Ok(analyzer);
                    }
                    _ => {}
                }
            }
            return Err("PE has no text section".to_string());
        }
        _ => return Err("Invalid file format".to_string()),
    }
}

pub fn open_db(path: &str) -> Result<Analyzer, String> {
    let path = PathBuf::from(path);
    let mut file = File::open(&path).map_err(|e| e.to_string())?;
    let mut buf = vec![];
    file.read_to_end(&mut buf).expect("Failed to read file");
    deserialize(&buf[..]).map_err(|e| e.to_string())
}

pub fn save_db(analyzer: &Analyzer, path: &str) -> Result<(), String> {
    let path = PathBuf::from(path);
    let mut file = File::create(&path).map_err(|e| e.to_string())?;
    let buf = serialize(analyzer).map_err(|e| e.to_string())?;
    file.write_all(&buf[..]).map_err(|e| e.to_string())?;
    Ok(())
}

pub fn decompile_function(graph: &InstructionGraph, address: u64) -> String {
    let il = decompile(graph, address);
    let blocks = build_flow_graph(il);
    let (blocks, types) = TypeResolver::resolve_types(blocks);
    let func = AstBuilder::build_ast(format!("sub_{0:06x}", address), &blocks, &types);
    CppEmitter::emit(&func)
}
