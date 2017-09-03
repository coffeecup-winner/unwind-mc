module rec Analyzer

open System
open System.Collections.Generic
open System.Linq
open NDis86
open NLog
open Graphs
open InstructionExtensions

type FunctionStatus =
    | Created
    | BoundsResolved
    | BoundsNotResolvedInvalidAddress
    | BoundsNotResolvedIncompleteGraph

type Function = {
    address: uint64
    mutable status: FunctionStatus
}

module JumpTable =
    type T = {
        reference: uint64
        address: uint64
        mutable firstIndex: int
        mutable count: int
    }

    let create (reference: uint64) (address: uint64): T = {
        reference = reference
        address = address
        firstIndex = 0
        count = 0
    }

let private logger = LogManager.GetCurrentClassLogger()

type T = {
    graph: InstructionGraph.T
    importResolver: IImportResolver.IImportResolver
    functions: SortedDictionary<uint64, Function>
    jumpTables: SortedDictionary<uint64, JumpTable.T>
}

let create (textBytes: ArraySegment<byte>) (pc: uint64) (importResolver: IImportResolver.IImportResolver): T = {
    graph = InstructionGraph.disassemble textBytes pc
    importResolver = importResolver
    functions = new SortedDictionary<uint64, Function>()
    jumpTables = new SortedDictionary<uint64, JumpTable.T>()
}

let analyze (t: T): unit =
    addExplicitCalls t
    resolveFunctionBounds t
    resolveExternalFunctionCalls t

let getGraph ({ graph = graph }): InstructionGraph.T = graph

let addFunction (t: T) (address: uint64): unit =
    t.functions.Add(address, { address = address; status = Created })

let private addExplicitCalls (t: T): unit =
    logger.Info("Adding explicit calls")
    for instr in t.graph.Instructions do
        if instr.Code = MnemonicCode.Icall && instr.Operands.[0].Type = OperandType.ImmediateBranch then
            let targetAddress = instr.GetTargetAddress()
            t.graph.AddLink(instr.Offset, targetAddress, InstructionGraph.LinkType.Call)
            if not (t.functions.ContainsKey(targetAddress)) then
                t.functions.Add(targetAddress, { address = targetAddress; status = Created })
    logger.Info("Done")

let private resolveFunctionBounds (t: T): unit =
    logger.Info("Resolving function bounds")
    let mutable index = 0
    let keys = t.functions.Keys
    let rec run (keys: uint64 list): unit =
        match keys with
        | key :: rest ->
            let func = t.functions.[key]
            index <- index + 1
            logger.Debug("[{0}/{1}] sub_{2:x8}...", index, t.functions.Count, func.address)
            if not (t.graph.InBounds(func.address)) then
                logger.Warn("The specified address is outside of code segment");
                func.status <- BoundsNotResolvedInvalidAddress
                run rest
            else
                if not (t.graph.ContainsAddress(func.address)) then
                    logger.Debug("The specified address is not a valid start of instruction, re-disassembling");
                    t.graph.Redisassemble(func.address);
                let visitedAllLinks =
                    t.graph.AsGenericGraph()
                        |> Graph.withEdgeFilter (fun e -> (e.type_ &&& InstructionGraph.LinkType.Next ||| InstructionGraph.LinkType.Branch ||| InstructionGraph.LinkType.SwitchCaseJump) <> InstructionGraph.LinkType.None)
                        |> Graph.dfsWith func.address (fun instr _ ->
                            t.graph.GetExtraData(instr.Offset).functionAddress <- func.address
                            if instr.Code = MnemonicCode.Iret then
                                false
                            else
                                addNextLinks t instr
                                addExplicitBranches t instr
                                addSwitchCases t instr
                                true
                        )
                func.status <- if visitedAllLinks then BoundsResolved else BoundsNotResolvedIncompleteGraph
        | [] -> ()
    run (keys |> Seq.toList)
    logger.Info("Done")

let private resolveExternalFunctionCalls (t: T): unit =
    logger.Info("Resolving external calls")
    for instr in t.graph.Instructions do
        let import =
            (tryGetImportAddress t instr 1)
            |> Option.orElse (tryGetImportAddress t instr 0)
            |> Option.map (fun i -> t.importResolver.GetImportName i)
        match import with
        | Some name ->
            t.graph.GetExtraData(instr.Offset).importName <- name
        | None -> ()
    logger.Info("Done")

let private tryGetImportAddress (t: T) (instr: Instruction) (index: int): uint64 option =
    if instr.Operands.Count > index && instr.Operands.[index].Type = OperandType.Memory &&
        instr.Operands.[index].Base = OperandType.None && instr.Operands.[index].Index = OperandType.None &&
        t.importResolver.IsImportAddress((uint64)instr.Operands.[index].LValue.udword) then
        Some instr.Operands.[index].LValue.uqword
    else
        None

let private addNextLinks (t: T) (instr: Instruction): unit =
    match instr.Code with
        | MnemonicCode.Iret
        | MnemonicCode.Ijmp
        | MnemonicCode.Iint3 -> ()
        | _ ->
            let next = t.graph.GetNext(instr.Offset)
            if next <> t.graph.FirstAddressAfterCode then
                t.graph.AddLink(instr.Offset, next, InstructionGraph.LinkType.Next)

let private addExplicitBranches (t: T) (instr: Instruction): unit =
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
                t.graph.AddLink(instr.Offset, instr.GetTargetAddress(), InstructionGraph.LinkType.Branch)
        | _ -> ()

let private addSwitchCases (t: T) (instr: Instruction): unit =
    if instr.Code = MnemonicCode.Ijmp && instr.Operands.[0].Type = OperandType.Memory then
        let address = (uint64)instr.Operands.[0].LValue.udword
        let table =
            match t.jumpTables.TryGetValue(address) with
            | (true, table) -> table
            | (false, _) ->
                let table = JumpTable.create instr.Offset address
                resolveJumpTable t table
                t.jumpTables.Add(address, table)
                table
        for i in [table.firstIndex .. table.count - 1] do
            t.graph.AddLink(table.reference, t.graph.ReadUInt32(table.address + (uint64)(i * 4)), InstructionGraph.LinkType.SwitchCaseJump)

let private resolveJumpTable (t: T) (table: JumpTable.T): unit =
    if table.address >= t.graph.FirstAddressAfterCode then
        logger.Warn("Jump table is not in the code segment")
    else
        logger.Info("Resolving jump table tbl_{0:x8}", table.address)
        let mutable idx = OperandType.None
        let mutable lowByteIdx = OperandType.None
        let mutable indirectAddress = 0uL
        let casesCountOption =
            t.graph.AsGenericGraph()
                |> Graph.withEdgeFilter (fun e -> (e.type_ &&& InstructionGraph.LinkType.Next ||| InstructionGraph.LinkType.Branch) <> InstructionGraph.LinkType.None)
                |> Graph.reverseEdges
                |> Graph.dfsPick table.reference (fun instr link ->
                    // find out the jump index register
                    if idx = OperandType.None then
                        idx <- instr.Operands.[0].Index
                        lowByteIdx <- getLowByteRegisterFromDword idx
                        Continue
                    else
                        // update the main jump register if the jump is indirect
                        if indirectAddress = 0uL && instr.Code = MnemonicCode.Imov && instr.Operands.[0].Base = lowByteIdx then
                            idx <- instr.Operands.[1].Base
                            indirectAddress <- (uint64)instr.Operands.[1].LValue.udword
                            Continue
                        else
                            // search for the jump to the default case
                            if not (instr.Code = MnemonicCode.Ija && instr.Operands.[0].Type = OperandType.ImmediateBranch) then
                                Continue
                            else
                                // search for cases count, can find it from code like cmp ecx, 0xb
                                // the jump register is irrelevant since it must be the closest one to ja
                                let value = AssignmentTracker.find t.graph instr.Offset idx (fun i _ ->
                                    i.Code = MnemonicCode.Icmp && i.Operands.[0].Type = OperandType.Register)
                                Return (value |> Option.map (fun v -> (int)v.ubyte + 1))
                )

        match casesCountOption with
        | Some casesCount ->
            let mutable (jumpsCount, casesCount) =
                if indirectAddress = 0uL then
                    (casesCount, 0)
                else
                    ((int)(t.graph.GetBytes(indirectAddress, casesCount).Max()) + 1, casesCount)

            let mutable offset = 0uL
            for i in [0 .. jumpsCount - 1] do
                if not (t.graph.AddJumpTableEntry(table.address + offset)) then
                    table.firstIndex <- table.firstIndex + 1
                t.graph.AddJumpTableEntry(table.address + offset) |> ignore
                offset <- offset + 4uL
            table.count <- jumpsCount
            while casesCount >= 4 do
                t.graph.AddJumpTableIndirectEntries(table.address + offset, 4)
                casesCount <- casesCount - 4
                offset <- offset + 4uL
            if casesCount > 0 then
                t.graph.AddJumpTableIndirectEntries(table.address + offset, casesCount)
                offset <- offset + (uint64)casesCount
            t.graph.Redisassemble(table.address + offset)
        | None -> ()
        logger.Info("Done")

let private getLowByteRegisterFromDword (register: OperandType): OperandType =
    match register with
        | OperandType.EAX -> OperandType.AL
        | OperandType.EBX -> OperandType.BL
        | OperandType.ECX -> OperandType.CL
        | OperandType.EDX -> OperandType.DL
        | _ -> OperandType.None
