use asm::InstructionGraph;
use ast_builder::AstBuilder;
use cpp_emitter::CppEmitter;
use flow_analyzer::build_flow_graph;
use il_decompiler::decompile;
use type_resolver::TypeResolver;

pub fn decompile_function(graph: &InstructionGraph, address: u64) -> String {
    let il = decompile(graph, address).expect("Failed to decompile IL");
    let blocks = build_flow_graph(il);
    let (blocks, types) = TypeResolver::resolve_types(blocks);
    let func = AstBuilder::build_ast(format!("sub_{0:06x}", address), &blocks, &types);
    CppEmitter::emit(&func)
}
