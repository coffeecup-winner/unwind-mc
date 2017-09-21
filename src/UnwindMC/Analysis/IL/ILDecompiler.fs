module rec ILDecompiler

open System
open System.Collections.Generic
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

let decompile (graph: InstructionGraph.T) (address: uint64): IReadOnlyList<ILInstruction<ILOperand>> =
    let t = {
        stackObjects = new Dictionary<int, obj>()
        stackOffset = -Constants.RegisterSize // skip the return address on the stack
        framePointerOffset = 0
        prevInstr = null
    }
    t.stackObjects.Add(t.stackOffset, null)
    let instructions = new Dictionary<uint64, ILInstruction<ILOperand>>()
    graph.AsGenericGraph()
        |> Graph.withEdgeFilter (fun e ->
            match e.type_ with
            | InstructionGraph.LinkType.Next
            | InstructionGraph.LinkType.Branch
            | InstructionGraph.LinkType.SwitchCaseJump ->
                true
            | _ ->
                false
        )
        |> Graph.dfs address
        |> Seq.iter (fun instr ->
            let ilInstructions = convertInstruction t instr
            if ilInstructions.Length > int instr.Length then
                FIXME "not enough virtual addresses"
            for i in [0 .. ilInstructions.Length - 1] do
                instructions.Add(instr.Offset + uint64 i, ilInstructions.[i])
        )
    let il = new SortedList<uint64, ILInstruction<ILOperand>>(instructions)
    let addresses = new Dictionary<ILInstruction<ILOperand>, uint64>(instructions.Count)
    for pair in il do
        addresses.[pair.Value] <- pair.Key
    let res = new List<ILInstruction<ILOperand>>()
    for pair in il do
        match pair.Value with
        | Branch branch ->
            res.Add(Branch { branch with target = uint64 (il.IndexOfKey(branch.target)) })
        | instr ->
            res.Add(instr)
    res :> IReadOnlyList<ILInstruction<ILOperand>>

let private convertInstruction (t: T) (instr: Instruction): ILInstruction<ILOperand>[] =
    let result =
        match instr.Code with
        | MnemonicCode.Iadd ->
            let operands = convertOperands t instr.Operands
            [| Add <| binary operands.[0] operands.[1] |]
        | MnemonicCode.Iand ->
            let operands = convertOperands t instr.Operands
            [| And <| binary operands.[0] operands.[1] |]
        | MnemonicCode.Icall ->
            let operands = convertOperands t instr.Operands
            [| Call <| unary operands.[0] |]
        | MnemonicCode.Icdq ->
            [||] // Size extension is irrelevant at the moment
        | MnemonicCode.Icmovl ->
            let operands = convertOperands t instr.Operands
            [|
                Branch <| branch GreaterOrEqual (instr.Offset + uint64 instr.Length)
                Assign <| binary operands.[0] operands.[1]
            |]
        | MnemonicCode.Icmp ->
            let operands = convertOperands t instr.Operands
            [| Compare <| binary operands.[0] operands.[1] |]
        | MnemonicCode.Idec ->
            let operands = convertOperands t instr.Operands
            [| Subtract <| binary operands.[0] (Value 1) |]
        | MnemonicCode.Iidiv ->
            let operands = convertOperands t instr.Operands
            [| Divide <| binary (Register OperandType.EAX) operands.[0] |]
        | MnemonicCode.Iimul ->
            let operands = convertOperands t instr.Operands
            [| Multiply <| binary operands.[0] operands.[1] |]
        | MnemonicCode.Iinc ->
            let operands = convertOperands t instr.Operands
            [| Add <| binary operands.[0] (Value 1) |]
        | MnemonicCode.Ija
        | MnemonicCode.Ijg ->
            [| Branch { type_ = Greater; target = instr.GetTargetAddress() } |]
        | MnemonicCode.Ijae
        | MnemonicCode.Ijge ->
            [| Branch { type_ = GreaterOrEqual; target = instr.GetTargetAddress() } |]
        | MnemonicCode.Ijb
        | MnemonicCode.Ijl ->
            [| Branch { type_ = Less; target = instr.GetTargetAddress() } |]
        | MnemonicCode.Ijbe
        | MnemonicCode.Ijle ->
            [| Branch { type_ = LessOrEqual; target = instr.GetTargetAddress() } |]
        | MnemonicCode.Ijmp ->
            [| Branch { type_ = Unconditional; target = instr.GetTargetAddress() } |]
        | MnemonicCode.Ijz ->
            let cond = tryGetVirtualConditionInstruction t
            let branch = Branch { type_ = Equal; target = instr.GetTargetAddress() }
            match cond with
            | Some instr -> [| instr; branch |]
            | None -> [| branch |]
        | MnemonicCode.Ijnz ->
            let cond = tryGetVirtualConditionInstruction t
            let branch = Branch { type_ = NotEqual; target = instr.GetTargetAddress() }
            match cond with
            | Some instr -> [| instr; branch |]
            | None -> [| branch |]
        | MnemonicCode.Imov ->
            let operands = convertOperands t instr.Operands
            match (operands.[0], operands.[1]) with
            | (Register OperandType.EBP, Register OperandType.ESP) ->
                t.framePointerOffset <- t.stackOffset
                [||]
            | (Register OperandType.ESP, Register OperandType.EBP) ->
                t.stackOffset <- t.framePointerOffset
                [||]
            | (Register OperandType.EBP, _)
            | (Register OperandType.ESP, _) ->
                notSupported
            | _ ->
                [| Assign <| binary operands.[0] operands.[1] |]
        | MnemonicCode.Ineg ->
            let operands = convertOperands t instr.Operands
            [| Negate <| unary operands.[0] |]
        | MnemonicCode.Inot ->
            let operands = convertOperands t instr.Operands
            [| Not <| unary operands.[0] |]
        | MnemonicCode.Ior ->
            let operands = convertOperands t instr.Operands
            [| Or <| binary operands.[0] operands.[1] |]
        | MnemonicCode.Ipush ->
            t.stackOffset <- t.stackOffset - Constants.RegisterSize
            addOrUpdateStackValue t t.stackOffset
            [| Nop |]
        | MnemonicCode.Ipop ->
            t.stackOffset <- t.stackOffset + Constants.RegisterSize
            [| Nop |]
        | MnemonicCode.Iret ->
            t.stackOffset <- t.stackOffset + Constants.RegisterSize
            if t.stackOffset <> 0 then
                failwith "Stack imbalance"
            [| Return <| unary (Register OperandType.EAX) |]
        | MnemonicCode.Ishl ->
            let operands = convertOperands t instr.Operands
            [| ShiftLeft <| binary operands.[0] operands.[1] |]
        | MnemonicCode.Isar ->
            let operands = convertOperands t instr.Operands
            [| ShiftRight <| binary operands.[0] operands.[1] |]
        | MnemonicCode.Isub ->
            let operands = convertOperands t instr.Operands
            [| Subtract <| binary operands.[0] operands.[1] |]
        | MnemonicCode.Itest ->
            let operands = convertOperands t instr.Operands
            match (operands.[0], operands.[1]) with
            | (Register regLeft, Register regRight) when regLeft = regRight ->
                [| Compare <| binary operands.[0] (Value 0) |]
            | _ ->
                notSupportedWith <| sprintf "Instruction `%O` is not supported yet" instr
        | MnemonicCode.Ixor ->
            let operands = convertOperands t instr.Operands
            [| Xor <| binary operands.[0] operands.[1] |]
        | _ -> notSupportedWith <| sprintf "Instruction `%O` is not supported yet" instr
    t.prevInstr <- instr
    result

let private tryGetVirtualConditionInstruction (t: T): ILInstruction<ILOperand> option =
    if t.prevInstr.Code = MnemonicCode.Icmp || t.prevInstr.Code = MnemonicCode.Itest then
        None
    else
        match t.prevInstr.Code with
        | MnemonicCode.Idec
        | MnemonicCode.Imov -> // TODO: this line is a heuristic and might be wrong in some cases
            let operands = convertOperands t t.prevInstr.Operands
            Some <| (Compare <| binary operands.[0] (Value 0))
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
            Stack (t.stackOffset + int (operand.GetMemoryOffset()))
        elif operand.Base = OperandType.EBP then
            Stack (t.framePointerOffset + int (operand.GetMemoryOffset()))
        elif operand.Index = OperandType.None then
            Pointer (operand.Base, int (operand.GetMemoryOffset()))
        else
            notSupported
    | OperandType.Constant
    | OperandType.Immediate ->
        Value (int (operand.GetValue()))
    | _ -> notSupported
