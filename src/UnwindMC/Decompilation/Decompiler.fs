module Decompiler

open System.IO
open System.Collections.Generic
open UnwindMC.Analysis
open UnwindMC.Analysis.IL
open UnwindMC.Analysis.Imports
open UnwindMC.Decompilation

let decompile (project : DecompilationProject.T): unit =
    let pe = PEFile.load project.exePath
    let importResolver = new ImportResolver(pe.imageBase, pe.getImportAddressTableBytes(), pe.getImportBytes())
    let analyzer = new Analyzer(pe.getTextBytes(), pe.textSectionAddress, importResolver)
    analyzer.AddFunction(pe.entryPointAddress)
    analyzer.Analyze()
    let data = ResultDumper.create analyzer.Graph analyzer.Functions
    File.WriteAllText(project.outputPath, ResultDumper.dumpResults data)
    File.WriteAllText(Path.Combine(project.rootPath, "functions.gv"), ResultDumper.dumpFunctionCallGraph data)
    let func = analyzer.Functions.[0x4afa88uL]
    func.ResolveBody(analyzer.Graph)
    func.ResolveTypes()
    // func.BuildAst()
    // File.WriteAllText(Path.Combine(project.rootPath, "sub_4afa88.gv"), ResultDumper.dumpILGraph func.FirstInstruction)

let decompileFunction (analyzer: Analyzer) (address: uint64): string =
    let blocks = FlowAnalyzer.buildFlowGraph(ILDecompiler.Decompile(analyzer.Graph, address))
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
