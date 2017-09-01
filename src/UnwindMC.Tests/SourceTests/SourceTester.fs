module rec SourceTester

open System
open System.IO
open System.Linq
open NUnit.Framework

let testDecompiler (code: string) (expected: string): unit =
    let code = trim code

    printfn "===================================== CODE ====================================="
    printfn "%s" code
    printfn ""

    let asm = disassemble code

    printfn "===================================== ASM ======================================"
    printfn "%s" asm
    printfn ""

    let cppCode = Decompiler.decompileFunction (AnalysisHelper.analyze asm) 0uL

    printfn "==================================== RESULT ===================================="
    printfn "%s" cppCode
    printfn ""

    Assert.That(cppCode, Is.EqualTo(trim(expected)))

let private disassemble (test: string): string =
    use tempDir = VcTools.CreateTempDirectory()
    let cppPath = Path.Combine(tempDir.Path, "test.cpp")
    File.WriteAllText(cppPath, test)
    let objPath = VcTools.compile(cppPath)
    VcTools.disassemble(objPath)

let private trim (expected: string): string =
    let lines = expected.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
    let indent = lines.[0].TakeWhile(fun c -> c = ' ').Count()
    System.String.Join(Environment.NewLine, lines.Select(fun l -> l.Substring(indent)))
