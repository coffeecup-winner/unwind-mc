module rec Analyzer

open System
open System.Collections.Generic
open System.Linq
open NDis86
open NLog
open UnwindMC.Analysis.Asm
open UnwindMC.Analysis.Imports
open UnwindMC.Collections
open UnwindMC.Util

type FunctionStatus
    = Created
    | BoundsResolved
    | BoundsNotResolvedInvalidAddress
    | BoundsNotResolvedIncompleteGraph

type Function = {
    address: uint64
    mutable status: FunctionStatus
}

let private Logger = LogManager.GetCurrentClassLogger()

type T = {
    _graph: InstructionGraph
    _importResolver: IImportResolver
    _functions: SortedDictionary<uint64, Function>
    _jumpTables: SortedDictionary<uint64, JumpTable>
}

let create (textBytes: ArraySegment<byte>) (pc: uint64) (importResolver: IImportResolver): T = {
    _graph = new InstructionGraph(textBytes, pc)
    _importResolver = importResolver
    _functions = new SortedDictionary<uint64, Function>()
    _jumpTables = new SortedDictionary<uint64, JumpTable>()
}

let analyze (t: T): unit =
    AddExplicitCalls t
    ResolveFunctionBounds t
    ResolveExternalFunctionCalls t

let getGraph ({ _graph = graph }): InstructionGraph = graph

let AddFunction (t: T) (address: uint64): unit =
    t._functions.Add(address, { address = address; status = Created })

let private AddExplicitCalls (t: T): unit =
    Logger.Info("Adding explicit calls")
    for instr in t._graph.Instructions do
        if instr.Code = MnemonicCode.Icall && instr.Operands.[0].Type = OperandType.ImmediateBranch then
            let targetAddress = instr.GetTargetAddress()
            t._graph.AddLink(instr.Offset, targetAddress, InstructionGraph.LinkType.Call)
            if not (t._functions.ContainsKey(targetAddress)) then
                t._functions.Add(targetAddress, { address = targetAddress; status = Created })
    Logger.Info("Done")

let private ResolveFunctionBounds (t: T): unit =
    Logger.Info("Resolving function bounds")
    let mutable index = 0
    let keys = t._functions.Keys
    let rec run (keys: uint64 list): unit =
        match keys with
        | key :: rest ->
            let func = t._functions.[key]
            index <- index + 1
            Logger.Debug("[{0}/{1}] sub_{2:x8}...", index, t._functions.Count, func.address)
            if not (t._graph.InBounds(func.address)) then
                Logger.Warn("The specified address is outside of code segment");
                func.status <- BoundsNotResolvedInvalidAddress
                run rest
            else
                if not (t._graph.Contains(func.address)) then
                    Logger.Debug("The specified address is not a valid start of instruction, re-disassembling");
                    t._graph.Redisassemble(func.address);
                let visitedAllLinks =
                    t._graph
                        .WithEdgeFilter(fun e -> (e.Type &&& InstructionGraph.LinkType.Next ||| InstructionGraph.LinkType.Branch ||| InstructionGraph.LinkType.SwitchCaseJump) <> InstructionGraph.LinkType.None)
                        .DFS(func.address, fun instr link ->
                            t._graph.GetExtraData(instr.Offset).FunctionAddress <- func.address
                            if instr.Code = MnemonicCode.Iret then
                                false
                            else
                                AddNextLinks t instr
                                AddExplicitBranches t instr
                                AddSwitchCases t instr
                                true
                        )
                func.status <- if visitedAllLinks then BoundsResolved else BoundsNotResolvedIncompleteGraph
        | [] -> ()
    run (keys |> Seq.toList)
    Logger.Info("Done")

let private ResolveExternalFunctionCalls (t: T): unit =
    Logger.Info("Resolving external calls")
    for instr in t._graph.Instructions do
        let import =
            (TryGetImportAddress t instr 1)
                .OrElse(fun () -> TryGetImportAddress t instr 0)
                .Map(fun i -> t._importResolver.GetImportName i)
        let hasValue, name = import.TryGet()
        if hasValue then
            t._graph.GetExtraData(instr.Offset).ImportName <- name
    Logger.Info("Done")

let private TryGetImportAddress (t: T) (instr: Instruction) (index: int): Option<uint64> =
    if instr.Operands.Count > index && instr.Operands.[index].Type = OperandType.Memory &&
        instr.Operands.[index].Base = OperandType.None && instr.Operands.[index].Index = OperandType.None &&
        t._importResolver.IsImportAddress((uint64)instr.Operands.[index].LValue.udword) then
        Option.Some(instr.Operands.[index].LValue.uqword)
    else
        Option<uint64>.None

let private AddNextLinks (t: T) (instr: Instruction): unit =
    match instr.Code with
        | MnemonicCode.Iret
        | MnemonicCode.Ijmp
        | MnemonicCode.Iint3 -> ()
        | _ ->
            let next = t._graph.GetNext(instr.Offset)
            if next <> t._graph.FirstAddressAfterCode then
                t._graph.AddLink(instr.Offset, next, InstructionGraph.LinkType.Next)

let private AddExplicitBranches (t: T) (instr: Instruction): unit =
    match instr.Code with
        | MnemonicCode.Ija
        | MnemonicCode.Ijae
        | MnemonicCode.Ijb
        | MnemonicCode.Ijbe
        | MnemonicCode.Ijcxz
        | MnemonicCode.Ijecxz
        | MnemonicCode.Ijg
        | MnemonicCode.Ijge
        | MnemonicCode.Ijl
        | MnemonicCode.Ijle
        | MnemonicCode.Ijmp
        | MnemonicCode.Ijno
        | MnemonicCode.Ijnp
        | MnemonicCode.Ijns
        | MnemonicCode.Ijnz
        | MnemonicCode.Ijo
        | MnemonicCode.Ijp
        | MnemonicCode.Ijrcxz
        | MnemonicCode.Ijs
        | MnemonicCode.Ijz ->
            if instr.Operands.[0].Type = OperandType.ImmediateBranch then
                t._graph.AddLink(instr.Offset, instr.GetTargetAddress(), InstructionGraph.LinkType.Branch)
        | _ -> ()

let private AddSwitchCases (t: T) (instr: Instruction): unit =
    if instr.Code <> MnemonicCode.Ijmp || instr.Operands.[0].Type <> OperandType.Memory then
        ()
    else
        let address = (uint64)instr.Operands.[0].LValue.udword
        let table =
            match t._jumpTables.TryGetValue(address) with
            | (true, table) -> table
            | (false, _) ->
                let table = new JumpTable(instr.Offset, address)
                ResolveJumpTable t table
                t._jumpTables.Add(address, table)
                table
        for i in [table.FirstIndex .. table.Count - 1] do
            t._graph.AddLink(table.Reference, t._graph.ReadUInt32(table.Address + (uint64)(i * 4)), InstructionGraph.LinkType.SwitchCaseJump)

let private ResolveJumpTable (t: T) (table: JumpTable): unit =
    if table.Address >= t._graph.FirstAddressAfterCode then
        Logger.Warn("Jump table is not in the code segment")
    else
        Logger.Info("Resolving jump table tbl_{0:x8}", table.Address)
        let mutable idx = OperandType.None
        let mutable lowByteIdx = OperandType.None
        let mutable indirectAddress = 0uL
        let mutable casesCountOption = Option<int>.None
        t._graph
            .WithEdgeFilter(fun e -> (e.Type &&& InstructionGraph.LinkType.Next ||| InstructionGraph.LinkType.Branch) <> InstructionGraph.LinkType.None)
            .ReverseEdges()
            .DFS(table.Reference, fun instr link ->
            // find out the jump index register
            if idx = OperandType.None then
                idx <- instr.Operands.[0].Index
                lowByteIdx <- RegisterHelper.GetLowByteRegisterFromDword(idx)
                true
            else
                // update the main jump register if the jump is indirect
                if indirectAddress = 0uL && instr.Code = MnemonicCode.Imov && instr.Operands.[0].Base = lowByteIdx then
                    idx <- instr.Operands.[1].Base
                    indirectAddress <- (uint64)instr.Operands.[1].LValue.udword
                    true
                else
                    // search for the jump to the default case
                    if not (instr.Code = MnemonicCode.Ija && instr.Operands.[0].Type = OperandType.ImmediateBranch) then
                        true
                    else
                        // search for cases count, can find it from code like cmp ecx, 0xb
                        // the jump register is irrelevant since it must be the closest one to ja
                        let tracker = new AssignmentTracker(t._graph)
                        let value = tracker.Find(instr.Offset, idx, fun i _ ->
                            i.Code = MnemonicCode.Icmp && i.Operands.[0].Type = OperandType.Register)
                        casesCountOption <- value.Map(fun v -> (int)v.ubyte + 1)
                        false
            ) |> ignore

        let hasValue, casesCount = casesCountOption.TryGet()
        if not hasValue then
            Logger.Info("Done")
        else
            let mutable (jumpsCount, casesCount) =
                if indirectAddress = 0uL then
                    (casesCount, 0)
                else
                    ((int)(t._graph.GetBytes(indirectAddress, casesCount).Max()) + 1, casesCount)

            let mutable offset = 0uL
            for i in [0 .. jumpsCount - 1] do
                if not (t._graph.AddJumpTableEntry(table.Address + offset)) then
                    table.FirstIndex <- table.FirstIndex
                t._graph.AddJumpTableEntry(table.Address + offset) |> ignore
                offset <- offset + 4uL
            table.Count <- jumpsCount
            while casesCount >= 4 do
                t._graph.AddJumpTableIndirectEntries(table.Address + offset, 4)
                casesCount <- casesCount - 4
                offset <- offset + 4uL
            if casesCount > 0 then
                t._graph.AddJumpTableIndirectEntries(table.Address + offset, casesCount)
                offset <- offset + (uint64)casesCount
            t._graph.Redisassemble(table.Address + offset)
            Logger.Info("Done")
