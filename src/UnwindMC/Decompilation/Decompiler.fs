module Decompiler

open System.Collections.Generic
open UnwindMC.Analysis.Asm

let decompile (project : DecompilationProject.T): unit =
    let pe = PEFile.load project.exePath
    let importResolver = new ImportResolver.ImportResolver(pe.imageBase, pe.getImportAddressTableBytes(), pe.getImportBytes())
    let analyzer = Analyzer.create (pe.getTextBytes()) pe.textSectionAddress importResolver
    Analyzer.addFunction analyzer pe.entryPointAddress
    Analyzer.analyze analyzer
    // TODO


let decompileFunction (graph: InstructionGraph) (address: uint64): string =
    let blocks = FlowAnalyzer.buildFlowGraph(ILDecompiler.decompile graph address)
    let result = TypeResolver.resolveTypes(blocks)
    let ast = AstBuilder.buildAst blocks result.parameterTypes result.localTypes result.variableTypes
    // TODO: these should be returned from AST step
    let types = new Dictionary<string, Type.DataType>()
    for i in [0 .. result.parameterTypes.Count - 1] do
        types.Add("arg" + string(i), result.parameterTypes.[i])
    for i in [0 .. result.localTypes.Count - 1] do
        types.Add("loc" + string(i), result.variableTypes.[i])
    for i in [0 .. result.variableTypes.Count - 1] do
        types.Add("var" + string(i), result.variableTypes.[i])
    CppEmitter.emit (System.String.Format("sub_{0:x6}", address)) types result.parameterTypes.Count ast
