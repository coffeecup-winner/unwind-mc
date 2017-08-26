module Decompiler

open System.IO;
open UnwindMC.Analysis;
open UnwindMC.Analysis.Imports;
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
