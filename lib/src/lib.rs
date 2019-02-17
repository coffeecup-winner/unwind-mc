#![feature(box_patterns)]
#![feature(nll)]
#![feature(slice_patterns)]
extern crate bincode;
#[macro_use]
extern crate bitflags;
extern crate either;
extern crate goblin;
extern crate libudis86_sys;
#[macro_use]
extern crate log;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate time;

pub mod analyzer;
pub mod api;
pub mod asm;
pub mod assignment_tracker;
pub mod ast;
pub mod ast_builder;
pub mod common;
pub mod cpp_emitter;
pub mod decompiler;
pub mod disassembler;
pub mod flow_analyzer;
pub mod function;
pub mod il;
pub mod il_decompiler;
pub mod text_writer;
pub mod transformer;
pub mod transformer_fixup_pointer_arithmetics;
pub mod transformer_fixup_zero_assignment;
pub mod type_resolver;

#[cfg(test)]
mod tests {
    use analyzer::Analyzer;

    #[test]
    fn create_empty_analyzer() {
        let mut analyzer = match Analyzer::create(vec![90], 0) {
            Ok(analyzer) => analyzer,
            Err(_) => panic!("Failed to create an analyzer."),
        };
        analyzer.analyze();
    }
}
