module Decompiler

let decompile (project : DecompilationProject.T): unit =
    let pe = PEFile.load project.exePath
    let importResolver = new ImportResolver.ImportResolver(pe.imageBase, pe.getImportAddressTableBytes(), pe.getImportBytes())
    let analyzer = Analyzer.create (pe.getTextBytes()) pe.textSectionAddress importResolver
    Analyzer.addFunction analyzer pe.entryPointAddress
    Analyzer.analyze analyzer
    FIXME "implement the rest of the project decompilation"

let decompileFunction (graph: InstructionGraph.T) (address: uint64): string =
    let il = ILDecompiler.decompile graph address
    let blocks = FlowAnalyzer.buildFlowGraph(il)
    let result = TypeResolver.resolveTypes(blocks)
    let func = AstBuilder.buildAst (sprintf "sub_%06x" address) blocks result.parameterTypes result.localTypes result.variableTypes
    CppEmitter.emit func
