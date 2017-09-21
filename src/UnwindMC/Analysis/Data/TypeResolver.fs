﻿module rec TypeResolver

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

let resolveTypes (blocks: IReadOnlyList<Block<ILOperand>>): Result =
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
    blocks
    |> convert
        (fun marker ->
            match marker with
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
        (fun instr ->
            match instr with
            | Negate unary
            | Not unary ->
                unary.operandId <- getCurrentId t unary.operand
                instr
            | Add binary
            | And binary
            | Compare binary
            | Divide binary
            | Multiply binary
            | Or binary
            | ShiftLeft binary
            | ShiftRight binary
            | Subtract binary
            | Xor binary ->
                let rightId = getCurrentId t binary.right
                match binary.right with
                | Value _ ->
                    if not (typeExists t binary.left) then
                        assignTypeBuilder t binary.left NoOperand
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
                binary.leftId <- getCurrentId t binary.left
                binary.rightId <- rightId
                instr
            | Assign binary ->
                let operand =
                    match binary.right with
                    | ILOperand.Pointer (reg, _) -> Register reg
                    | _ -> binary.right
                if not (typeExists t operand) then
                    match operand with
                    | Stack _
                    | Value _ ->
                        ()
                    | _ ->
                        failwith "Right side of assignment does not have a type assigned"
                binary.rightId <- getCurrentId t operand
                assignTypeBuilder t binary.left operand
                match binary.right with
                | ILOperand.Pointer _ ->
                    let type_ = ref !(getType t operand)
                    (getType t operand) := Pointer <| type_
                    setType t binary.left type_
                | _ -> ()
                binary.leftId <- getCurrentId t binary.left
                instr
            | Call unary ->
                (getType t unary.operand) := Function
                unary.operandId <- getCurrentId t unary.operand
                instr
            | Return unary ->
                if functionReturnsValue blocks then
                    unary.operandId <- getCurrentId t unary.operand
                instr
            | Break
            | Branch _
            | Nop ->
                instr
        )
    |> ignore
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
    for pair in t.coalescedIds |> Seq.rev do
        blocks
        |> convert
            (fun _ -> ())
            (fun instr ->
                match instr with
                | Negate unary
                | Not unary
                | Call unary
                | Return unary ->
                    if unary.operandId = pair.Key then
                        unary.operandId <- pair.Value
                | Add binary
                | And binary
                | Compare binary
                | Divide binary
                | Multiply binary
                | Or binary
                | ShiftLeft binary
                | ShiftRight binary
                | Subtract binary
                | Xor binary
                | Assign binary ->
                    if binary.leftId = pair.Key then
                        binary.leftId <- pair.Value
                    if binary.rightId = pair.Key then
                        binary.rightId <- pair.Value
                | Break
                | Branch _
                | Nop ->
                    ()
                instr
            )
        |> ignore
    {
        parameterTypes = parameterTypes
        localTypes = localTypes
        variableTypes = t.variableTypes
    }

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
    | Stack offset ->
        if offset >= 0 then t.parameterTypes.ContainsKey(offset) else t.localTypes.ContainsKey(offset)
    | _ ->
        t.types.ContainsKey(operand)

let private getType (t: T) (operand: ILOperand): TypeBuilder ref =
    match operand with
    | Stack offset ->
        if offset >= 0 then t.parameterTypes.[offset] else t.localTypes.[offset]
    | _ ->
        t.types.[operand]

let private setType (t: T) (operand: ILOperand) (type_: TypeBuilder ref): unit =
    match operand with
    | Stack offset ->
        (if offset >= 0 then t.parameterTypes else t.localTypes).[offset] <- type_
    | _ ->
        t.types.[operand] <- type_

let private finalizeType (t: T) (operand: ILOperand): unit =
    let id = getCurrentId t operand
    while t.variableTypes.Count <= id do
        t.variableTypes.Add(DataType.Int32)
    t.variableTypes.[id] <- t.types.[operand] |> build
    t.types.Remove(operand) |> ignore

let private assignTypeBuilder (t: T) (target: ILOperand) (source: ILOperand): unit =
    let typeBuilder =
        match source with
        | NoOperand
        | Value _ -> ref Fresh
        | Stack offset ->
            let types = if offset >= 0 then t.parameterTypes else t.localTypes
            match getValue types offset with
            | Some type_ ->
                type_
            | None ->
                let type_ = ref Fresh
                types.Add(offset, type_)
                type_
        | _ -> getType t source
    match target with
    | Stack offset ->
        if offset >= 0 then
            t.parameterTypes.[offset] <- typeBuilder
        else
            t.localTypes.[offset] <- typeBuilder
    | _ ->
        match getValue t.types target with
        | Some _ ->
            finalizeType t target
        | None -> ()
        t.types.[target] <- typeBuilder
        t.currentIds.[target] <- t.nextId
        t.nextId <- t.nextId + 1

let private getCurrentId (t: T) (op: ILOperand): int =
    match op with
    | Register _
    | ILOperand.Pointer _ ->
        let id = t.currentIds.[op]
        coalesceIds t id
        id
    | _ -> -1

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
