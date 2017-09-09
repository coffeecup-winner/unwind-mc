module rec TypeResolver

open System
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
    types: Dictionary<int, Dictionary<ILOperand, TypeBuilder>>
    parameterTypes: Dictionary<int, TypeBuilder>
    localTypes: Dictionary<int, TypeBuilder>
    currentIds: Dictionary<ILOperand, int>
    mutable currentLevel: int
    mutable nextId: int
}

type private ScopeBoundsMarker = Start | End

type private TypeBuilder = {
    mutable isFunction: bool
    mutable indirectionLevel: int
}

let private build (typeBuilder: TypeBuilder): DataType = {
    isFunction = typeBuilder.isFunction
    indirectionLevel = typeBuilder.indirectionLevel
    size = Constants.RegisterSize
}

let resolveTypes (blocks: IReadOnlyList<Block>): Result =
    let t = {
        types = new Dictionary<int, Dictionary<ILOperand, TypeBuilder>>()
        parameterTypes = new Dictionary<int, TypeBuilder>()
        localTypes = new Dictionary<int, TypeBuilder>()
        currentIds = new Dictionary<ILOperand, int>()
        currentLevel = 0
        nextId = 0
    }
    let variableTypes = new List<DataType>()
    let typesToRemove = new Dictionary<int, Dictionary<ILOperand, int>>()
    typesToRemove.Add(0, new Dictionary<ILOperand, int>())
    t.types.[t.currentLevel] <- new Dictionary<ILOperand, TypeBuilder>()
    for item in traverseReversed blocks do
        match item with
        | Left marker ->
            match marker with
            | ScopeBoundsMarker.Start ->
                for pair in typesToRemove.[t.currentLevel] do
                    if getScopeLevel t pair.Key = t.currentLevel then
                        finalizeType t variableTypes pair.Value pair.Key
                typesToRemove.Remove(t.currentLevel) |> ignore
                for pair in t.types.[t.currentLevel] do
                    t.types.[t.currentLevel - 1].Add(pair.Key, pair.Value)
                t.types.Remove(t.currentLevel) |> ignore
                t.currentLevel <- t.currentLevel - 1
            | ScopeBoundsMarker.End ->
                t.currentLevel <- t.currentLevel + 1
                t.types.[t.currentLevel] <- new Dictionary<ILOperand, TypeBuilder>()
                typesToRemove.[t.currentLevel] <- new Dictionary<ILOperand, int>()
        | Right instr ->
            match instr with
            | Negate unary
            | Not unary ->
                unary.operandId <- getCurrentId t.currentIds unary.operand
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
                if typesToRemove.[t.currentLevel].ContainsKey(binary.left) then
                    typesToRemove.[t.currentLevel].Remove(binary.left) |> ignore
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
                binary.leftId <- getCurrentId t.currentIds binary.left
                binary.rightId <- getCurrentId t.currentIds binary.right
            | Assign binary ->
                if not (typeExists t binary.left) then
                    match binary.left with
                    | Stack _ -> ()
                    | _ -> failwith "Assignment appears to not be used in the execution path"
                let operand =
                    match binary.right with
                    | Pointer (reg, _) -> Register reg
                    | _ -> binary.right
                match binary.right with
                | Value _ ->
                    if not (typeExists t binary.left) then
                        assignTypeBuilder t binary.left NoOperand
                | _ ->
                    if typeExists t operand then
                        let type_ = getType t binary.left |> build
                        let operandType = getType t operand
                        if type_.isFunction then
                            operandType.isFunction <- true
                        operandType.indirectionLevel <- type_.indirectionLevel
                    else
                        if not (typeExists t binary.left) then
                            assignTypeBuilder t binary.left NoOperand
                        assignTypeBuilder t operand binary.left
                match binary.right with
                | Pointer _ ->
                    (getType t operand).indirectionLevel <- (getType t operand).indirectionLevel + 1
                | _ -> ()
                binary.leftId <- getCurrentId t.currentIds binary.left
                binary.rightId <- getCurrentId t.currentIds operand
                match binary.left with
                | Stack _ -> ()
                | _ ->
                    if (t.currentLevel = 0) then
                        finalizeType t variableTypes binary.leftId binary.left
                    else
                        typesToRemove.[t.currentLevel].Add(binary.left, binary.leftId)
            | Call unary ->
                if typesToRemove.[t.currentLevel].ContainsKey(unary.operand) then
                    typesToRemove.[t.currentLevel].Remove(unary.operand) |> ignore
                if not (typeExists t unary.operand) then
                    assignTypeBuilder t unary.operand NoOperand
                (getType t unary.operand).isFunction <- true
                unary.operandId <- getCurrentId t.currentIds unary.operand
            | Return unary ->
                if functionReturnsValue blocks then
                    assignTypeBuilder t unary.operand NoOperand
                    unary.operandId <- getCurrentId t.currentIds unary.operand
            | Branch _
            | Nop ->
                ()
    let parameterTypes = new List<DataType>();
    for pair in t.parameterTypes.OrderBy(fun x -> x.Key) do
        parameterTypes.Add(pair.Value |> build)
    let localTypes = new List<DataType>()
    for pair in t.localTypes.OrderByDescending(fun x -> x.Key) do
        localTypes.Add(pair.Value |> build)
    {
        parameterTypes = parameterTypes
        localTypes = localTypes
        variableTypes = variableTypes
    }

let private functionReturnsValue (blocks: IReadOnlyList<Block>): bool =
    // This tests only the top-level scope, which is probably an incomplete heuristic,
    // most probably need to check all paths
    blocks
    |> Seq.collect (
        function
        | SequentialBlock { instructions = is } -> is |> Seq.toList
        | _ -> []
    )
    |> Seq.exists (function Assign { left = Register OperandType.EAX } -> true | _ -> false)

let private getScopeLevel (t: T) (operand: ILOperand): int =
    match operand with
    | Stack _ -> 0
    | _ ->
        [t.currentLevel .. -1 .. 0]
        |> Seq.find (fun i -> t.types.[i].ContainsKey(operand))

let private typeExists (t: T) (operand: ILOperand): bool =
    match operand with
    | Stack offset ->
        if offset >= 0 then t.parameterTypes.ContainsKey(offset) else t.localTypes.ContainsKey(offset)
    | _ ->
        [t.currentLevel .. -1 .. 0]
        |> Seq.exists (fun i -> t.types.[i].ContainsKey(operand))

let private getType (t: T) (operand: ILOperand): TypeBuilder =
    match operand with
    | Stack offset ->
        if offset >= 0 then t.parameterTypes.[offset] else t.localTypes.[offset]
    | _ ->
        [t.currentLevel .. -1 .. 0]
        |> Seq.find (fun i -> t.types.[i].ContainsKey(operand))
        |> fun i -> t.types.[i].[operand]

let private finalizeType (t: T) (variableTypes: List<DataType>) (id: int) (operand: ILOperand): unit =
    while variableTypes.Count <= id do
        variableTypes.Add({ isFunction = false; indirectionLevel = 0; size = 0 }) // TODO: check if needed
    variableTypes.[id] <- t.types.[t.currentLevel].[operand] |> build
    t.types.[t.currentLevel].Remove(operand) |> ignore
    t.currentIds.Remove(operand) |> ignore

let private assignTypeBuilder (t: T) (target: ILOperand) (source: ILOperand): unit =
    let typeBuilder =
        if source = NoOperand then
            { isFunction = false; indirectionLevel = 0 }
        else
            match source with
            | Stack offset -> if offset >= 0 then t.parameterTypes.[offset] else t.localTypes.[offset]
            | _ -> getType t source
    match target with
    | Stack offset ->
        if offset >= 0 then
            t.parameterTypes.[offset] <- typeBuilder
        else
            t.localTypes.[offset] <- typeBuilder
    | _ ->
        t.types.[t.currentLevel].[target] <- typeBuilder
        t.currentIds.[target] <- t.nextId
        t.nextId <- t.nextId + 1

let private getCurrentId (currentIds: IReadOnlyDictionary<ILOperand, int>) (op: ILOperand): int =
    if op = NoOperand then
        -1
    else
        match op with
        | Register _
        | Pointer _ ->
            currentIds.[op]
        | _ -> -1

type private Traversal =
    | Marker of ScopeBoundsMarker
    | Instructions of IReadOnlyList<ILInstruction>
    | Block of Block

let private traverseReversed (blocks: IReadOnlyList<Block>): IEnumerable<Either<ScopeBoundsMarker, ILInstruction>> =
    seq {
        let stack = new Stack<Traversal>(blocks |> Seq.map Block)
        while stack.Count > 0 do
            match stack.Pop() with
            | Marker scopeBoundsMarker ->
                yield Left scopeBoundsMarker
            | Instructions instructions ->
                for i in [0 .. instructions.Count - 1] |> Seq.rev do
                    yield Right(instructions.[i])
            | Block block ->
                match block with
                | SequentialBlock { instructions = instructions } ->
                    for i in [0 .. instructions.Count - 1] |> Seq.rev do
                        yield Right(instructions.[i])
                | WhileBlock { condition = condition; children = children } ->
                    stack.Push(Marker Start)
                    stack.Push(Instructions condition)
                    for child in children do
                        stack.Push(Block child)
                    stack.Push(Marker End)
                | DoWhileBlock { condition = condition; children = children } ->
                    stack.Push(Marker Start)
                    for child in children do
                        stack.Push(Block child)
                    stack.Push(Instructions condition)
                    stack.Push(Marker End)
                | ForBlock { condition = condition; modifier = modifier; body = body } ->
                    stack.Push(Marker Start)
                    for child in body do
                        stack.Push(Block child)
                    stack.Push(Instructions condition)
                    stack.Push(Instructions modifier)
                    stack.Push(Marker End)
                | ConditionalBlock { condition = condition; trueBranch = trueBranch; falseBranch = falseBranch } ->
                    stack.Push(Instructions condition)
                    stack.Push(Marker Start)
                    for child in trueBranch do
                        stack.Push(Block child)
                    stack.Push(Marker End)
                    stack.Push(Marker Start)
                    for child in falseBranch do
                        stack.Push(Block child)
                    stack.Push(Marker End)
    }
