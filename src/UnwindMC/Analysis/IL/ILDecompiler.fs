module rec ILDecompiler

open System
open System.Collections.Generic
open System.Linq
open NDis86
open Graphs
open IL
open InstructionExtensions

type private T = {
    stackObjects: Dictionary<int, obj>
    mutable stackOffset: int
    mutable framePointerOffset: int
    mutable prevInstr: Instruction
}

let decompile (graph: InstructionGraph.T) (address: uint64): ILInstruction =
    let t = {
        stackObjects = new Dictionary<int, obj>()
        stackOffset = -4 // skip the return address on the stack
        framePointerOffset = 0
        prevInstr = null
    }
    t.stackObjects.Add(t.stackOffset, null)
    let instructions = new Dictionary<uint64, ILInstruction>()
    graph.AsGenericGraph()
        |> Graph.withEdgeFilter (fun e -> (e.type_ &&& InstructionGraph.LinkType.Next ||| InstructionGraph.LinkType.Branch ||| InstructionGraph.LinkType.SwitchCaseJump) <> InstructionGraph.LinkType.None)
        |> Graph.dfs address
        |> Seq.iter (fun instr ->
            let ilInstructions = convertInstruction t instr
            if ilInstructions.Length > (int)instr.Length then
                FIXME "not enough virtual addresses"
            for i in [0 .. ilInstructions.Length - 1] do
                instructions.Add(instr.Offset + (uint64)i, ilInstructions.[i])
        )
    let il = new SortedList<uint64, ILInstruction>(instructions)
    let addresses = new Dictionary<ILInstruction, uint64>(instructions.Count)
    let mutable order = 0
    for pair in il do
        pair.Value.order <- order
        order <- order + 1
        addresses.[pair.Value] <- pair.Key
    let mutable current: ILInstruction option = None
    let mutable lastBranch: ILBranch option = None
    let mutable isEmptyThen = false
    let rec run (instructions: ILInstruction list): unit =
        match instructions with
        | instr :: rest ->
            if instr.type_ = Nop then
                run rest
            elif current.IsNone then
                current <- Some instr
                run rest
            elif instr.type_ = Virtual then
                let mutable index = il.IndexOfKey(instr.branch.address)
                while il.Values.[index].type_ = Nop do
                    index <- index + 1
                current.Value.defaultChild <- Some (il.Values.[index])
                if instr.branch.type_ = Next then
                    if lastBranch.IsSome then
                        isEmptyThen <- true
                    else
                        current <- None
                else
                    lastBranch <- Some instr.branch
                run rest
            else
                match lastBranch with
                | Some branch ->
                    let branchType = if isEmptyThen then complement branch.type_ else branch.type_
                    if addresses.[current.Value.defaultChild.Value] > addresses.[instr] then
                        current.Value.condition <- complement branchType
                        current.Value.conditionalChild <- Some instr
                    else
                        current.Value.condition <- branchType
                        current.Value.conditionalChild <- current.Value.defaultChild
                        current.Value.defaultChild <- Some instr
                    current <- Some instr
                    lastBranch <- Option.None
                    isEmptyThen <- false
                    run rest
                | _ ->
                    current.Value.defaultChild <- Some instr
                    current <- Some instr
                    run rest
        | [] -> ()
    run (il |> Seq.map (fun p -> p.Value) |> Seq.toList)
    il.Values.First(fun i -> i.type_ <> Nop && i.type_ <> Virtual)

let private convertInstruction (t: T) (instr: Instruction): ILInstruction[] =
    let result =
        match instr.Code with
        | MnemonicCode.Iadd ->
            let operands = convertOperands t instr.Operands
            [| createBinaryInstruction Add operands.[0] operands.[1] |]
        | MnemonicCode.Iand ->
            let operands = convertOperands t instr.Operands
            [| createBinaryInstruction And operands.[0] operands.[1] |]
        | MnemonicCode.Icall ->
            let operands = convertOperands t instr.Operands
            [| createUnaryInstruction Call operands.[0] |]
        | MnemonicCode.Icdq ->
            [||] // Size extension is irrelevant at the moment
        | MnemonicCode.Icmovl ->
            let operands = convertOperands t instr.Operands
            [|
                createBranchInstruction ILBranchType.GreaterOrEqual (instr.Offset + (uint64)instr.Length)
                createBinaryInstruction Assign operands.[0] operands.[1]
            |]
        | MnemonicCode.Icmp ->
            let operands = convertOperands t instr.Operands
            [| createBinaryInstruction Compare operands.[0] operands.[1] |]
        | MnemonicCode.Idec ->
            let operands = convertOperands t instr.Operands
            [| createBinaryInstruction Subtract operands.[0] (Value 1) |]
        | MnemonicCode.Iidiv ->
            let operands = convertOperands t instr.Operands
            [| createBinaryInstruction Divide (Register OperandType.EAX) operands.[0] |]
        | MnemonicCode.Iimul ->
            let operands = convertOperands t instr.Operands
            [| createBinaryInstruction Multiply operands.[0] operands.[1] |]
        | MnemonicCode.Ijae ->
            [| createBranchInstruction ILBranchType.GreaterOrEqual (instr.GetTargetAddress()) |]
        | MnemonicCode.Ijmp ->
            [| createBranchInstruction ILBranchType.Next (instr.GetTargetAddress()) |]
        | MnemonicCode.Ijz ->
            let cond = tryGetVirtualConditionInstruction t
            let branch = createBranchInstruction ILBranchType.Equal (instr.GetTargetAddress())
            match cond with
            | Some instr -> [| instr; branch |]
            | None -> [| branch |]
        | MnemonicCode.Ijnz ->
            let cond = tryGetVirtualConditionInstruction t
            let branch = createBranchInstruction ILBranchType.NotEqual (instr.GetTargetAddress())
            match cond with
            | Some instr -> [| instr; branch |]
            | None -> [| branch |]
        | MnemonicCode.Imov ->
            let operands = convertOperands t instr.Operands
            if isRegister operands.[0] OperandType.EBP then
                if not (isRegister operands.[1] OperandType.ESP) then
                    notSupported
                t.framePointerOffset <- t.stackOffset
                [||]
            elif isRegister operands.[0] OperandType.ESP then
                if not (isRegister operands.[1] OperandType.EBP) then
                    notSupported
                t.stackOffset <- t.framePointerOffset
                [||]
            else
                [| createBinaryInstruction Assign operands.[0] operands.[1] |]
        | MnemonicCode.Ineg ->
            let operands = convertOperands t instr.Operands
            [| createUnaryInstruction Negate operands.[0] |]
        | MnemonicCode.Inot ->
            let operands = convertOperands t instr.Operands
            [| createUnaryInstruction Not operands.[0] |]
        | MnemonicCode.Ior ->
            let operands = convertOperands t instr.Operands
            [| createBinaryInstruction Or operands.[0] operands.[1] |]
        | MnemonicCode.Ipush ->
            t.stackOffset <- t.stackOffset - 4
            addOrUpdateStackValue t t.stackOffset
            [| createNullaryInstruction Nop |]
        | MnemonicCode.Ipop ->
            t.stackOffset <- t.stackOffset + 4
            [| createNullaryInstruction Nop |]
        | MnemonicCode.Iret ->
            t.stackOffset <- t.stackOffset + 4
            if t.stackOffset <> 0 then
                failwith "Stack imbalance"
            [| createBinaryInstruction Return NoOperand (Register OperandType.EAX) |]
        | MnemonicCode.Ishl ->
            let operands = convertOperands t instr.Operands
            [| createBinaryInstruction ShiftLeft operands.[0] operands.[1] |]
        | MnemonicCode.Isar ->
            let operands = convertOperands t instr.Operands
            [| createBinaryInstruction ShiftRight operands.[0] operands.[1] |]
        | MnemonicCode.Isub ->
            let operands = convertOperands t instr.Operands
            [| createBinaryInstruction Subtract operands.[0] operands.[1] |]
        | MnemonicCode.Itest ->
            let operands = convertOperands t instr.Operands
            match operands.[0] with
            | Register reg when isRegister operands.[1] reg ->
                [| createBinaryInstruction Compare operands.[0] (Value 0) |]
            | _ ->
                notSupportedWith <| sprintf "Instruction `%O` is not supported yet" instr
        | MnemonicCode.Ixor ->
            let operands = convertOperands t instr.Operands
            [| createBinaryInstruction Xor operands.[0] operands.[1] |]
        | _ -> notSupportedWith <| sprintf "Instruction `%O` is not supported yet" instr
    t.prevInstr <- instr
    result

let private tryGetVirtualConditionInstruction (t: T): ILInstruction option =
    if t.prevInstr.Code = MnemonicCode.Icmp || t.prevInstr.Code = MnemonicCode.Itest then
        None
    else
        match t.prevInstr.Code with
        | MnemonicCode.Idec
        | MnemonicCode.Imov -> // TODO: this line is a heuristic and might be wrong in some cases
            let operands = convertOperands t t.prevInstr.Operands
            Some <| createBinaryInstruction Compare operands.[0] (Value 0)
        | _ -> notSupported

let private convertOperands (t: T) (operands: IReadOnlyList<Operand>): ILOperand[] =
    let result: ILOperand[] = Array.zeroCreate operands.Count
    for i in [0 .. operands.Count - 1] do
        let op = convertOperand t operands.[i]
        match op with
        | Stack offset -> addOrUpdateStackValue t offset
        | _ -> ()
        result.[i] <- op
    result

let private addOrUpdateStackValue (t: T) (offset: int): unit =
    let hasValue, _ = t.stackObjects.TryGetValue(offset)
    if not hasValue then
        t.stackObjects.Add(offset, null)

let private convertOperand (t: T) (operand: Operand): ILOperand =
    match operand.Type with
    | OperandType.Register ->
        Register operand.Base
    | OperandType.Memory ->
        if operand.Base = OperandType.ESP then
            Stack (t.stackOffset + (int)(operand.GetMemoryOffset()))
        elif operand.Base = OperandType.EBP then
            Stack (t.framePointerOffset + (int)(operand.GetMemoryOffset()))
        elif operand.Index = OperandType.None then
            Pointer (operand.Base, (int)(operand.GetMemoryOffset()))
        else
            notSupported
    | OperandType.Constant
    | OperandType.Immediate ->
        Value ((int)(operand.GetValue()))
    | _ -> notSupported

let private complement (type_: ILBranchType): ILBranchType =
    match type_ with
    | ILBranchType.Equal -> ILBranchType.NotEqual
    | ILBranchType.NotEqual -> ILBranchType.Equal
    | ILBranchType.Less -> ILBranchType.GreaterOrEqual
    | ILBranchType.LessOrEqual -> ILBranchType.Greater
    | ILBranchType.GreaterOrEqual -> ILBranchType.Less
    | ILBranchType.Greater -> ILBranchType.LessOrEqual
    | _ -> failwith "Cannot find branch type complement"
