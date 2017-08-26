module rec TypeResolver

open System
open System.Collections.Generic
open System.Linq
open NDis86
open UnwindMC.Analysis.IL
open UnwindMC.Util
open Type
open FlowAnalyzer

type Result = {
    parameterTypes: IReadOnlyList<DataType>
    localTypes: IReadOnlyList<DataType>
    variableTypes: IReadOnlyList<DataType>
}

type private T = {
    types: Dictionary<int, Dictionary<ILOperand, TypeBuilder>>
    parameterTypes: Dictionary<ILOperand, TypeBuilder>
    localTypes: Dictionary<ILOperand, TypeBuilder>
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
    size = 4
}

let resolveTypes (blocks: IReadOnlyList<Block>): Result =
    let t = {
        types = new Dictionary<int, Dictionary<ILOperand, TypeBuilder>>()
        parameterTypes = new Dictionary<ILOperand, TypeBuilder>()
        localTypes = new Dictionary<ILOperand, TypeBuilder>()
        currentIds = new Dictionary<ILOperand, int>()
        currentLevel = 0
        nextId = 0
    }
    let variableTypes = new List<DataType>()
    let typesToRemove = new Dictionary<int, Dictionary<ILOperand, int>>()
    typesToRemove.Add(0, new Dictionary<ILOperand, int>())
    t.types.[t.currentLevel] <- new Dictionary<ILOperand, TypeBuilder>()
    for item in traverseReversed blocks do
        if item.IsLeft then
            match item.Left with
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
        else
            let instr = item.Right
            match instr.Type with
            | ILInstructionType.Negate
            | ILInstructionType.Not ->
                    instr.SetVariableIds(getCurrentId t.currentIds instr.Target, getCurrentId t.currentIds instr.Source)
            | ILInstructionType.Add
            | ILInstructionType.And
            | ILInstructionType.Compare
            | ILInstructionType.Divide
            | ILInstructionType.Multiply
            | ILInstructionType.Or
            | ILInstructionType.ShiftLeft
            | ILInstructionType.ShiftRight
            | ILInstructionType.Subtract
            | ILInstructionType.Xor ->
                if typesToRemove.[t.currentLevel].ContainsKey(instr.Target) then
                    typesToRemove.[t.currentLevel].Remove(instr.Target) |> ignore
                if instr.Source.Type = ILOperandType.Value then
                    if not (typeExists t instr.Target) then
                        assignTypeBuilder t instr.Target null
                else
                    if typeExists t instr.Source && typeExists t instr.Target then
                        if getType t instr.Source <> getType t instr.Target then
                            raise (new InvalidOperationException("Type mismatch"))
                    elif typeExists t instr.Source then
                        assignTypeBuilder t instr.Target instr.Source
                    elif typeExists t instr.Target then
                        assignTypeBuilder t instr.Source instr.Target
                    else
                        raise (new NotImplementedException())
                instr.SetVariableIds(getCurrentId t.currentIds instr.Target, getCurrentId t.currentIds instr.Source)
            | ILInstructionType.Assign ->
                if not (typeExists t instr.Target) then
                    if instr.Target.Type <> ILOperandType.Stack then
                        raise (new InvalidOperationException("Assignment appears to not be used in the execution path"))
                let operand =
                    if instr.Source.Type = ILOperandType.Pointer then
                        ILOperand.FromRegister(instr.Source.Register)
                    else
                        instr.Source
                if instr.Source.Type = ILOperandType.Value then
                    if not (typeExists t instr.Target) then
                        assignTypeBuilder t instr.Target null
                else
                    if typeExists t operand then
                        let type_ = getType t instr.Target |> build
                        let operandType = getType t operand
                        if type_.isFunction then
                            operandType.isFunction <- true
                        operandType.indirectionLevel <- type_.indirectionLevel
                    else
                        if not (typeExists t instr.Target) then
                            assignTypeBuilder t instr.Target null
                        assignTypeBuilder t operand instr.Target
                if instr.Source.Type = ILOperandType.Pointer then
                    (getType t operand).indirectionLevel <- (getType t operand).indirectionLevel + 1
                instr.SetVariableIds(getCurrentId t.currentIds instr.Target, getCurrentId t.currentIds operand)
                if instr.Target.Type <> ILOperandType.Stack then
                    if (t.currentLevel = 0) then
                        finalizeType t variableTypes instr.TargetId instr.Target
                    else
                        typesToRemove.[t.currentLevel].Add(instr.Target, instr.TargetId)
            | ILInstructionType.Call ->
                if typesToRemove.[t.currentLevel].ContainsKey(instr.Target) then
                    typesToRemove.[t.currentLevel].Remove(instr.Target) |> ignore
                if not (typeExists t instr.Target) then
                    assignTypeBuilder t instr.Target null
                (getType t instr.Target).isFunction <- true
                instr.SetVariableIds(getCurrentId t.currentIds instr.Target, getCurrentId t.currentIds instr.Source)
            | ILInstructionType.Return ->
                if functionReturnsValue blocks then
                    assignTypeBuilder t instr.Source null
                    instr.SetVariableIds(getCurrentId t.currentIds instr.Target, getCurrentId t.currentIds instr.Source)
            | _ -> raise (new InvalidOperationException("unknown instruction type"))
    let parameterTypes = new List<DataType>();
    for pair in t.parameterTypes.OrderBy(fun p -> p.Key.Offset) do
        parameterTypes.Add(pair.Value |> build)
    let localTypes = new List<DataType>()
    for pair in t.localTypes.OrderByDescending(fun p -> p.Key.Offset) do
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
    |> Seq.exists (fun i -> i.Target <> null && i.Target.IsRegister(OperandType.EAX))

let private getScopeLevel (t: T) (operand: ILOperand): int =
    if t.parameterTypes.ContainsKey(operand) || t.localTypes.ContainsKey(operand) then
        0
    else
        [0 .. t.currentLevel]
        |> Seq.rev
        |> Seq.find (fun i -> t.types.[i].ContainsKey(operand))

let private typeExists (t: T) (operand: ILOperand): bool =
    if operand.Type = ILOperandType.Stack then
        if operand.Offset >= 0 then t.parameterTypes.ContainsKey(operand) else t.localTypes.ContainsKey(operand)
    else
        [0 .. t.currentLevel]
        |> Seq.rev
        |> Seq.exists (fun i -> t.types.[i].ContainsKey(operand))

let private getType (t: T) (operand: ILOperand): TypeBuilder =
    if operand.Type = ILOperandType.Stack then
        if operand.Offset >= 0 then t.parameterTypes.[operand] else t.localTypes.[operand]
    else
        [0 .. t.currentLevel]
        |> Seq.rev
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
        if source = null then
            { isFunction = false; indirectionLevel = 0 }
        elif source.Type = ILOperandType.Stack then
            t.parameterTypes.[source]
        else
            getType t source
    if target.Type = ILOperandType.Stack then
        if target.Offset >= 0 then
            t.parameterTypes.[target] <- typeBuilder
        else
            t.localTypes.[target] <- typeBuilder
    else
        t.types.[t.currentLevel].[target] <- typeBuilder
        t.currentIds.[target] <- t.nextId
        t.nextId <- t.nextId + 1

let private getCurrentId (currentIds: IReadOnlyDictionary<ILOperand, int>) (op: ILOperand): int =
    if op = null then
        -1
    else
        match op.Type with
        | ILOperandType.Register
        | ILOperandType.Pointer ->
            currentIds.[op]
        | _ -> -1

let private traverseReversed (blocks: IReadOnlyList<Block>): IEnumerable<Either<ScopeBoundsMarker, ILInstruction>> =
    seq {
        let stack = new Stack<Object>() // replace with Either
        for block in blocks do
            stack.Push(block)
        while stack.Count > 0 do
            match stack.Pop() with
            | :? ScopeBoundsMarker as scopeBoundsMarker -> 
                yield Either.Left<ScopeBoundsMarker, ILInstruction>(scopeBoundsMarker)
            | :? ILInstruction as instr ->
                yield Either.Right<ScopeBoundsMarker, ILInstruction>(instr)
            | :? Block as block ->
                match block with
                | SequentialBlock { instructions = instructions } ->
                    for i in [0 .. instructions.Count - 1] |> Seq.rev do
                        yield Either.Right<ScopeBoundsMarker, ILInstruction>(instructions.[i])
                | WhileBlock { condition = condition; children = children } ->
                    stack.Push(ScopeBoundsMarker.Start)
                    stack.Push(condition)
                    for child in children do
                        stack.Push(child)
                    stack.Push(ScopeBoundsMarker.End)
                | DoWhileBlock { condition = condition; children = children } ->
                    stack.Push(ScopeBoundsMarker.Start)
                    for child in children do
                        stack.Push(child)
                    stack.Push(condition)
                    stack.Push(ScopeBoundsMarker.End)
                | ConditionalBlock { condition = condition; trueBranch = trueBranch; falseBranch = falseBranch } ->
                    stack.Push(condition)
                    stack.Push(ScopeBoundsMarker.Start)
                    for child in trueBranch do
                        stack.Push(child)
                    stack.Push(ScopeBoundsMarker.End)
                    stack.Push(ScopeBoundsMarker.Start)
                    for child in falseBranch do
                        stack.Push(child)
                    stack.Push(ScopeBoundsMarker.End)
            | _ -> raise (new InvalidOperationException("Unknown block type"))
    }
