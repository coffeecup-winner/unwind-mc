module rec TypeResolver

open System.Collections.Generic
open System.Linq
open NDis86
open IL
open FlowAnalyzer
open Type

type Result = {
    parameterTypes: IReadOnlyList<DataType>
    localTypes: IReadOnlyList<DataType>
    variableTypes: IReadOnlyList<DataType>
}

type private T = {
    types: Dictionary<ILOperand, TypeBuilder ref>
    parameterTypes: Dictionary<int, TypeBuilder ref>
    localTypes: Dictionary<int, TypeBuilder ref>
    variableTypes: List<DataType>
    currentIds: Dictionary<ILOperand, int>
    pushedIds: Stack<Dictionary<ILOperand, int>>
    sameIds: Dictionary<int, List<int>>
    coalescedIds: SortedDictionary<int, int>
    mutable nextId: int
}

type private TypeBuilder =
    | Fresh
    | Int32
    | Function
    | Pointer of (TypeBuilder ref)

let private build (typeBuilder: TypeBuilder ref): DataType =
    match !typeBuilder with
    | Fresh
    | Int32 -> DataType.Int32
    | Function -> DataType.Function
    | Pointer t -> DataType.Pointer <| build t

let resolveTypes (blocks: IReadOnlyList<Block<ILOperand>>): IReadOnlyList<Block<ResolvedOperand>> * Result =
    let t = {
        types = new Dictionary<ILOperand, TypeBuilder ref>()
        parameterTypes = new Dictionary<int, TypeBuilder ref>()
        localTypes = new Dictionary<int, TypeBuilder ref>()
        variableTypes = new List<DataType>()
        currentIds = new Dictionary<ILOperand, int>()
        pushedIds = new Stack<Dictionary<ILOperand, int>>()
        sameIds = new Dictionary<int, List<int>>()
        coalescedIds = new SortedDictionary<int, int>()
        nextId = 0
    }
    let returnsValue = functionReturnsValue blocks
    let blocks = 
        blocks
        |> convert
            (function
            | PushIds ->
                let copy = t.currentIds.ToDictionary((fun p -> p.Key), (fun p -> p.Value))
                t.pushedIds.Push(copy)
            | JoinPoint ->
                let ids1 = t.pushedIds.Pop()
                let ids0 = t.pushedIds.Pop()
                t.currentIds.Clear()
                for pair in ids0 do
                    t.currentIds.Add(pair.Key, pair.Value)
                for pair in ids1 do
                    match getValue t.currentIds pair.Key with
                    | Some value when value <> pair.Value ->
                        match getValue t.sameIds value with
                        | Some ids ->
                            ids.Add(pair.Value)
                        | None ->
                            let ids = new List<int>()
                            ids.Add(pair.Value)
                            t.sameIds.Add(value, ids)
                    | Some _ -> () // skip unchanged ids
                    | None -> t.currentIds.Add(pair.Key, pair.Value)
            )
            (function
            | Binary (op, binary) -> Binary (op, convertBinary t binary)
            | Unary (op, unary) -> Unary (op, convertUnary t unary)
            | Compare binary -> Compare <| convertBinary t binary
            | Assign binary -> Assign <| convertAssign t binary
            | Call unary -> Call <| convertCall t unary
            | Return unary -> Return <| convertReturn t unary returnsValue
            | Continue -> Continue
            | Break -> Break
            | Branch branch -> Branch branch
            | Nop -> Nop
            )
    let parameterTypes = new List<DataType>()
    for pair in t.parameterTypes.OrderBy(fun x -> x.Key) do
        parameterTypes.Add(pair.Value |> build)
    let localTypes = new List<DataType>()
    for pair in t.localTypes.OrderByDescending(fun x -> x.Key) do
        localTypes.Add(pair.Value |> build)
    while t.types.Count > 0 do
        let pair = t.types.First()
        let operand = pair.Key
        finalizeType t operand
    let blocks =
        t.coalescedIds
        |> Seq.rev
        |> Seq.fold (fun blocks pair ->
            let coalesceUnary (unary: UnaryInstruction<ResolvedOperand>): UnaryInstruction<ResolvedOperand> =
                let (op, opId) = unary.operand
                match opId with
                | Some id when id = pair.Key -> { unary with operand = (op, Some pair.Value) }
                | _ -> unary
            let coaleasceBinary (binary: BinaryInstruction<ResolvedOperand>): BinaryInstruction<ResolvedOperand> =
                let (left, leftId) = binary.left
                let leftId = if leftId = Some pair.Key then Some pair.Value else leftId
                let (right, rightId) = binary.right
                let rightId = if rightId = Some pair.Key then Some pair.Value else rightId
                { binary with left = (left, leftId); right = (right, rightId) }
            blocks
            |> convert
                (fun _ -> ())
                (function
                | Binary (op, binary) -> Binary (op, coaleasceBinary binary)
                | Unary (op, unary) -> Unary (op, coalesceUnary unary)
                | Call unary -> Call <| coalesceUnary unary
                | Return unary -> Return <| coalesceUnary unary
                | Compare binary -> Compare <| coaleasceBinary binary
                | Assign binary -> Assign <| coaleasceBinary binary
                | Continue -> Continue
                | Break -> Break
                | Branch branch -> Branch branch
                | Nop -> Nop
                )
            ) blocks
    let result =
        {
            parameterTypes = parameterTypes
            localTypes = localTypes
            variableTypes = t.variableTypes
        }
    (blocks, result)

let private convertUnary (t: T) (unary: UnaryInstruction<ILOperand>): UnaryInstruction<ResolvedOperand> =
    let { operand = operand } = unary
    { operand = (operand, getCurrentId t operand) }

let private convertBinary (t: T) (binary: BinaryInstruction<ILOperand>): BinaryInstruction<ResolvedOperand> =
    let rightId = getCurrentId t binary.right
    match binary.right with
    | Value _ ->
        if not (typeExists t binary.left) then
            assignTypeBuilder t binary.left (Value 0) // get a fresh type
    | _ ->
        if typeExists t binary.right && typeExists t binary.left then
            if getType t binary.right <> getType t binary.left then
                failwith "Type mismatch"
        elif typeExists t binary.right then
            assignTypeBuilder t binary.left binary.right
        elif typeExists t binary.left then
            assignTypeBuilder t binary.right binary.left
        else
            notSupported
    let { left = left; right = right } = binary
    { left = (left, getCurrentId t left); right = (right, rightId) }

let private convertAssign (t: T) (binary: BinaryInstruction<ILOperand>): BinaryInstruction<ResolvedOperand> =
    let operand =
        match binary.right with
        | ILOperand.Pointer (reg, _) -> Register reg
        | _ -> binary.right
    if not (typeExists t operand) then
        match operand with
        | Argument _
        | Local _
        | Value _ ->
            ()
        | _ ->
            failwith "Right side of assignment does not have a type assigned"
    let rightId = getCurrentId t operand
    assignTypeBuilder t binary.left operand
    match binary.right with
    | ILOperand.Pointer _ ->
        let type_ = ref !(getType t operand)
        (getType t operand) := Pointer <| type_
        setType t binary.left type_
    | _ -> ()
    let { left = left; right = right } = binary
    { left = (left, getCurrentId t left); right = (right, rightId) }

let private convertCall (t: T) (unary: UnaryInstruction<ILOperand>): UnaryInstruction<ResolvedOperand> =
    let { operand = operand } = unary
    (getType t operand) := Function
    { operand = (operand, getCurrentId t operand) }

let private convertReturn (t: T) (unary: UnaryInstruction<ILOperand>) (returnsValue: bool): UnaryInstruction<ResolvedOperand> =
    let { operand = operand } = unary
    if returnsValue then
        { operand = (operand, getCurrentId t operand) }
    else
        { operand = (operand, None) }

let private functionReturnsValue (blocks: IReadOnlyList<Block<ILOperand>>): bool =
    // This tests only the top-level scope, which is probably an incomplete heuristic,
    // most probably need to check all paths
    blocks
    |> Seq.collect (
        function
        | SequentialBlock { instructions = is } -> is |> Seq.toList
        | _ -> []
    )
    |> Seq.exists (function Assign { left = Register OperandType.EAX } -> true | _ -> false)

let private typeExists (t: T) (operand: ILOperand): bool =
    match operand with
    | Argument offset -> t.parameterTypes.ContainsKey(offset)
    | Local offset -> t.localTypes.ContainsKey(offset)
    | _ -> t.types.ContainsKey(operand)

let private getType (t: T) (operand: ILOperand): TypeBuilder ref =
    match operand with
    | Argument offset -> t.parameterTypes.[offset]
    | Local offset -> t.localTypes.[offset]
    | _ -> t.types.[operand]

let private setType (t: T) (operand: ILOperand) (type_: TypeBuilder ref): unit =
    match operand with
    | Argument offset -> t.parameterTypes .[offset] <- type_
    | Local offset -> t.localTypes.[offset] <- type_
    | _ -> t.types.[operand] <- type_

let private finalizeType (t: T) (operand: ILOperand): unit =
    let id =
      match getCurrentId t operand with
      | Some id -> id
      | None -> impossible
    while t.variableTypes.Count <= id do
        t.variableTypes.Add(DataType.Int32)
    t.variableTypes.[id] <- t.types.[operand] |> build
    t.types.Remove(operand) |> ignore

let private assignTypeBuilder (t: T) (target: ILOperand) (source: ILOperand): unit =
    let typeBuilder =
        match source with
        | Value _ -> ref Fresh
        | Argument offset ->
            match getValue t.parameterTypes offset with
            | Some type_ ->
                type_
            | None ->
                let type_ = ref Fresh
                t.parameterTypes.Add(offset, type_)
                type_
        | Local offset ->
            match getValue t.localTypes offset with
            | Some type_ ->
                type_
            | None ->
                let type_ = ref Fresh
                t.localTypes.Add(offset, type_)
                type_
        | _ -> getType t source
    match target with
    | Argument offset -> t.parameterTypes.[offset] <- typeBuilder
    | Local offset -> t.localTypes.[offset] <- typeBuilder
    | _ ->
        match getValue t.types target with
        | Some _ ->
            finalizeType t target
        | None -> ()
        t.types.[target] <- typeBuilder
        t.currentIds.[target] <- t.nextId
        t.nextId <- t.nextId + 1

let private getCurrentId (t: T) (op: ILOperand): int option =
    match op with
    | Register _
    | ILOperand.Pointer _ ->
        let id = t.currentIds.[op]
        coalesceIds t id
        Some id
    | _ -> None

let private coalesceIds (t: T) (id: int): unit =
    match getValue t.sameIds id with
    | Some ids ->
        for sameId in ids do
            match getValue t.coalescedIds sameId with
            | Some v when v = id -> ()
            | Some _ -> failwithf "Trying to coalesce %d with a new id" sameId
            | None ->
                t.coalescedIds.Add(sameId, id)
                coalesceIds t sameId
    | None -> ()

type private SsaMarker = PushIds | JoinPoint

let private convert<'opA, 'opB>
    (sendMarker: SsaMarker -> unit)
    (mapInstruction: ILInstruction<'opA> -> ILInstruction<'opB>)
    (blocks: IReadOnlyList<Block<'opA>>)
    : IReadOnlyList<Block<'opB>> =
    let convertInstructions = ROL.map mapInstruction
    let convertBlocks = convert sendMarker mapInstruction
    blocks
    |> ROL.map
        (function
        | SequentialBlock { instructions = instructions } ->
            SequentialBlock { instructions = convertInstructions instructions }
        | WhileBlock { condition = condition; body = body } ->
            sendMarker PushIds
            let condition = convertInstructions condition
            let body = convertBlocks body
            sendMarker PushIds
            sendMarker JoinPoint
            WhileBlock { condition = condition; body = body }
        | DoWhileBlock { condition = condition; body = body } ->
            sendMarker PushIds
            let body = convertBlocks body
            let condition = convertInstructions condition
            sendMarker PushIds
            sendMarker JoinPoint
            DoWhileBlock { condition = condition; body = body }
        | ForBlock { condition = condition; modifier = modifier; body = body } ->
            sendMarker PushIds
            let body = convertBlocks body
            let condition = convertInstructions condition
            let modifier = convertInstructions modifier
            sendMarker PushIds
            sendMarker JoinPoint
            ForBlock { condition = condition; modifier = modifier; body = body }
        | ConditionalBlock { condition = condition; trueBranch = trueBranch; falseBranch = falseBranch } ->
            let hasFalseBranch = falseBranch.Count > 0
            let hasTrueBranch = trueBranch.Count > 0
            let condition = convertInstructions condition
            if hasFalseBranch <> hasTrueBranch then
                sendMarker PushIds
            let trueBranch =
                if hasTrueBranch then
                    let trueBranch = convertBlocks trueBranch
                    sendMarker PushIds
                    trueBranch
                else
                    [||] :> IReadOnlyList<Block<'opB>>
            let falseBranch =
                if hasFalseBranch then
                    let falseBranch = convertBlocks falseBranch
                    sendMarker PushIds
                    falseBranch
                else
                    [||] :> IReadOnlyList<Block<'opB>>
            sendMarker JoinPoint
            ConditionalBlock { condition = condition; trueBranch = trueBranch; falseBranch = falseBranch }
        )
