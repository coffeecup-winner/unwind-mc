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

pub mod api;
pub mod asm_analyzer;
pub mod assignment_tracker;
pub mod ast;
pub mod ast_builder;
pub mod common;
pub mod cpp_emitter;
pub mod disassembler;
pub mod flow_analyzer;
pub mod il;
pub mod il_decompiler;
pub mod instruction_graph;
pub mod project;
pub mod text_writer;
pub mod transformer;
pub mod transformer_fixup_pointer_arithmetics;
pub mod transformer_fixup_zero_assignment;
pub mod type_resolver;
