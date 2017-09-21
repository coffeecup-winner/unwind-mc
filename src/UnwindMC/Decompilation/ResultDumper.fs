module ResultDumper

open System.Collections.Generic
open System.Text
open NDis86
open NLog
open Analyzer
open IL
open InstructionExtensions

let logger = LogManager.GetCurrentClassLogger()

type T = {
    graph: InstructionGraph.T
    functions: IDictionary<uint64, Function>
}

let create (graph: InstructionGraph.T) (functions: IDictionary<uint64, Function>): T =
    { graph = graph; functions = functions }

let dumpResults (t: T): string =
    logger.Info("Dumping results")
    let sb = new StringBuilder()
    let mutable unresolvedInstructions = 0
    let mutable incompleteInstructions = 0
    for instr in t.graph.Instructions do
        let address = t.graph.GetExtraData(instr.Offset).functionAddress
        let description =
            if address = 0uL then
                if instr.Code = MnemonicCode.Inop || instr.Code = MnemonicCode.Iint3 || instr.Assembly = "mov edi, edi" || instr.Assembly = "lea ecx, [ecx]" then
                    "--------"
                elif instr.Code = MnemonicCode.Inone then
                    "jmptable"
                else
                    unresolvedInstructions <- unresolvedInstructions + 1
                    "????????"
            elif t.functions.[address].status = BoundsNotResolvedIncompleteGraph then
                incompleteInstructions <- incompleteInstructions + 1
                "xxxxxxxx"
            else
                sprintf "%08x" address
        sb.AppendFormat("{0} {1:x8} {2,20} {3}", description, instr.Offset, instr.Hex, instr.Assembly) |> ignore
        let importName = t.graph.GetExtraData(instr.Offset).importName
        if importName <> null then
            sb.Append(" ; " + importName) |> ignore
        sb.AppendLine() |> ignore

    let result = sb.ToString()
    logger.Info("Done: {0} ({1:0%}) unresolved, {2} ({3:0%}) incomplete",
        unresolvedInstructions, (double)unresolvedInstructions / (double)t.graph.Instructions.Count,
        incompleteInstructions, (double)incompleteInstructions / (double)t.graph.Instructions.Count)
    result

let dumpFunctionCallGraph (t: T): string =
    logger.Info("Dumping function call graph")
    let sb = new StringBuilder()
    sb.AppendLine("digraph functions {") |> ignore
    for func in t.functions.Values do
        sb.AppendLine(System.String.Format("  sub_{0:x8}", func.address)) |> ignore
    for instr in t.graph.Instructions do
        if instr.Code = MnemonicCode.Icall && instr.Operands.[0].Type = OperandType.ImmediateBranch then
            sb.AppendLine(System.String.Format("  sub_{0:x8} -> sub_{1:x8}", t.graph.GetExtraData(instr.Offset).functionAddress, instr.GetTargetAddress())) |> ignore
    sb.AppendLine("}") |> ignore
    let result = sb.ToString()
    logger.Info("Done")
    result

let dumpILGraph (il: IReadOnlyList<ILInstruction<ILOperand>>): string =
    logger.Info("Dumping IL graph")
    let sb = new StringBuilder()
    sb.AppendLine("digraph il {") |> ignore
    for pair in il |> Seq.indexed do
        let (index, instr) = pair
        sb.AppendLine(System.String.Format("  {0} [label=\"{1}\"]", instr.GetHashCode(), instr.ToString())) |> ignore
        match instr with
        | Branch { target = target } ->
            sb.AppendLine(
                System.String.Format(
                    "  {0} -> {1} [label=\"{2}\"]",
                    instr.GetHashCode(),
                    il.[int target].GetHashCode(),
                    "true")) |> ignore
        | _ -> ()
    for pair in il |> Seq.windowed 2 do
        match pair with
        | [| (Branch { type_ = Unconditional }); _ |] -> ()
        | [| first; second |] ->
            sb.AppendLine(
                System.String.Format(
                    "  {0} -> {1} [label=\"\"]",
                    first.GetHashCode(),
                    second.GetHashCode())) |> ignore
        | _ -> impossible
    sb.AppendLine("}") |> ignore
    let result = sb.ToString()
    logger.Info("Done")
    result
