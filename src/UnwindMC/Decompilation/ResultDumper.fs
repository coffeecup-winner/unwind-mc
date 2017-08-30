module ResultDumper

open System.Collections.Generic
open System.Text
open NDis86
open NLog
open UnwindMC.Analysis.Asm
open Analyzer
open IL

let Logger: Logger = LogManager.GetCurrentClassLogger()

type T = {
    graph: InstructionGraph
    functions: IDictionary<uint64, Function>
}

let create (graph: InstructionGraph) (functions: IDictionary<uint64, Function>): T =
    { graph = graph; functions = functions }

let dumpResults (t: T): string =
    Logger.Info("Dumping results")
    let sb = new StringBuilder()
    let mutable unresolvedInstructions = 0
    let mutable incompleteInstructions = 0
    for instr in t.graph.Instructions do
        let address = t.graph.GetExtraData(instr.Offset).FunctionAddress
        let mutable description: string = null
        if address = 0uL then
            if instr.Code = MnemonicCode.Inop || instr.Code = MnemonicCode.Iint3 || instr.Assembly = "mov edi, edi" || instr.Assembly = "lea ecx, [ecx]" then
                description <- "--------"
            elif instr.Code = MnemonicCode.Inone then
                description <- "jmptable"
            else
                description <- "????????"
                unresolvedInstructions <- unresolvedInstructions + 1
        elif t.functions.[address].status = BoundsNotResolvedIncompleteGraph then
            description <- "xxxxxxxx"
            incompleteInstructions <- incompleteInstructions + 1
        else
            description <- System.String.Format("{0:x8}", address)
        sb.AppendFormat("{0} {1:x8} {2,20} {3}", description, instr.Offset, instr.Hex, instr.Assembly) |> ignore
        let importName = t.graph.GetExtraData(instr.Offset).ImportName
        if importName <> null then
            sb.Append(" ; " + importName) |> ignore
        sb.AppendLine() |> ignore

    let result = sb.ToString()
    Logger.Info("Done: {0} ({1:0%}) unresolved, {2} ({3:0%}) incomplete",
        unresolvedInstructions, (double)unresolvedInstructions / (double)t.graph.Instructions.Count,
        incompleteInstructions, (double)incompleteInstructions / (double)t.graph.Instructions.Count)
    result

let dumpFunctionCallGraph (t: T): string =
    Logger.Info("Dumping function call graph")
    let sb = new StringBuilder()
    sb.AppendLine("digraph functions {") |> ignore
    for func in t.functions.Values do
        sb.AppendLine(System.String.Format("  sub_{0:x8}", func.address)) |> ignore
    for instr in t.graph.Instructions do
        if instr.Code = MnemonicCode.Icall && instr.Operands.[0].Type = OperandType.ImmediateBranch then
            sb.AppendLine(System.String.Format("  sub_{0:x8} -> sub_{1:x8}", t.graph.GetExtraData(instr.Offset).FunctionAddress, instr.GetTargetAddress())) |> ignore
    sb.AppendLine("}") |> ignore
    let result = sb.ToString()
    Logger.Info("Done")
    result

let dumpILGraph (il: ILInstruction): string =
    Logger.Info("Dumping IL graph")
    let sb = new StringBuilder()
    sb.AppendLine("digraph il {") |> ignore
    let visited = new HashSet<ILInstruction>()
    let queue = new Queue<ILInstruction>()
    queue.Enqueue(il)
    visited.Add(il) |> ignore
    while queue.Count > 0 do
        let instr = queue.Dequeue()
        sb.AppendLine(System.String.Format("  {0} [label=\"{1}\"]", instr.GetHashCode(), instr.ToString())) |> ignore
        match instr.defaultChild with
        | Some child ->
            if visited.Add(child) then
                queue.Enqueue(child)
            sb.AppendLine(
                System.String.Format(
                    "  {0} -> {1} [label=\"{2}\"]",
                    instr.GetHashCode(),
                    child.GetHashCode(),
                    if instr.conditionalChild.IsNone then "" else "false")) |> ignore
        | _ -> ()
        match instr.conditionalChild with
        | Some child ->
            if visited.Add(child) then
                queue.Enqueue(child)
            sb.AppendLine(
                System.String.Format(
                    "  {0} -> {1} [label=\"{2}\"]",
                    instr.GetHashCode(),
                    child.GetHashCode(),
                    "true")) |> ignore
        | _ -> ()
    sb.AppendLine("}") |> ignore
    let result = sb.ToString()
    Logger.Info("Done")
    result
