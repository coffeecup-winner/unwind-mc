use asm::*;
use ast_builder::*;
use cpp_emitter::*;
use flow_analyzer::*;
use il_decompiler::*;
use type_resolver::*;

pub fn decompile_function(graph: &InstructionGraph, address: u64) -> String {
    let il = decompile(graph, address);
    let blocks = build_flow_graph(il);
    let (blocks, types) = TypeResolver::resolve_types(blocks);
    let func = AstBuilder::build_ast(format!("sub_{0:06x}", address), &blocks, &types);
    CppEmitter::emit(&func)
}
