module rec AstBuilder

open System.Collections.Generic
open Ast
open IL
open FlowAnalyzer
open Type

type private T = {
    blocks: IReadOnlyList<Block>
    variableTypes: IReadOnlyList<DataType>
    variableNames: Dictionary<int, string>
    parameterNames: Dictionary<int, string>
    localNames: Dictionary<int, string>
    types: Dictionary<string, DataType>
    mutable nextVariableNameIdx: int
}

type Variable = {
    name: string
    type_: DataType
}

type Function = {
    name: string
    parameters: IReadOnlyList<Variable>
    locals: IReadOnlyList<Variable>
    variables: IReadOnlyList<Variable>
    body: IReadOnlyList<Statement>
}

let buildAst (name: string) (blocks: IReadOnlyList<Block>) (parameterTypes: IReadOnlyList<DataType>)
    (localTypes: IReadOnlyList<DataType>) (variableTypes: IReadOnlyList<DataType>): Function =
    let t = {
        blocks = blocks
        variableTypes = variableTypes
        variableNames = new Dictionary<int, string>()
        parameterNames = new Dictionary<int, string>()
        localNames = new Dictionary<int, string>()
        types = new Dictionary<string, DataType>()
        nextVariableNameIdx = 0
    }
    let mutable offset = 0
    for index in [0 .. parameterTypes.Count - 1] do
        let name = "arg" + string(index)
        t.parameterNames.[offset] <- name
        t.types.[name] <- parameterTypes.[index]
        offset <- offset + parameterTypes.[index].size
    offset <- -Constants.RegisterSize * 2 // TODO: this will not always be correct
    for index in [0 .. localTypes.Count - 1] do
        offset <- offset - localTypes.[index].size
        let name = "loc" + string(index)
        t.localNames.[offset] <- name
        t.types.[name] <- localTypes.[index]
    let body =
        buildScope t t.blocks
        |> ROL.map (runTransformations t)
    {
        name = name
        parameters = t.parameterNames |> Seq.map (fun p -> { name = p.Value; type_ = t.types.[p.Value] }) |> Seq.toArray
        locals = t.localNames |> Seq.map (fun p -> { name = p.Value; type_ = t.types.[p.Value] }) |> Seq.toArray
        variables = t.variableNames |> Seq.map (fun p -> { name = p.Value; type_ = t.variableTypes.[p.Key] }) |> Seq.toArray
        body = body
    }

let private runTransformations (t: T) (ast: Statement): Statement =
    [
        FixupPointerArithmetics.transformer t.types
        FixupZeroAssignment.transformer
    ] |> Seq.fold (fun a t -> Transformer.transform t a) ast

let private buildScope (t: T) (blocks: IReadOnlyList<Block>): IReadOnlyList<Statement> =
    let statements = new List<Statement>()
    for block in blocks do
        match block with
        | SequentialBlock { instructions = is } ->
            for instr in is |> Seq.filter (function Nop -> false | _ -> true) do
                statements.Add(buildStatement t instr);
        | WhileBlock { condition = c; body = cs } ->
            statements.Add(buildWhile t c cs)
        | DoWhileBlock { condition = c; body = cs } ->
            statements.Add(buildDoWhile t c cs)
        | ForBlock { condition = c; modifier = m; body = b } ->
            statements.Add(buildFor t c m b)
        | ConditionalBlock { condition = c; trueBranch = tb; falseBranch = fb } ->
            statements.Add(buildIfThenElse t c tb fb)
    statements :> IReadOnlyList<Statement>

let private buildWhile (t: T) (condition: IReadOnlyList<ILInstruction>) (children: IReadOnlyList<Block>): Statement =
    While (buildCondition t condition, buildScope t children)

let private buildDoWhile (t: T) (condition: IReadOnlyList<ILInstruction>) (children: IReadOnlyList<Block>): Statement =
    DoWhile (buildScope t children, buildCondition t condition)

let private buildFor (t: T) (condition: IReadOnlyList<ILInstruction>) (modifier: IReadOnlyList<ILInstruction>) (body: IReadOnlyList<Block>): Statement =
    For (buildCondition t condition, buildScope t [| (SequentialBlock { instructions = modifier }) |], buildScope t body)

let private buildIfThenElse (t: T) (condition: IReadOnlyList<ILInstruction>) (trueBranch: IReadOnlyList<Block>) (falseBranch: IReadOnlyList<Block>): Statement =
    IfThenElse (buildCondition t condition, buildScope t trueBranch, buildScope t falseBranch)

let private buildCondition (t: T) (condition: IReadOnlyList<ILInstruction>): Expression =
    match (condition |> Seq.toList) with
    | (Compare compare) :: (Branch branch) :: [] ->
        buildBinaryOperator t (getBinaryOperator t branch.type_) compare
    | (Assign assign) :: (Compare compare) :: (Branch branch) :: [] when assign.left = compare.left ->
        // TODO: this is a special case where we can inline the assigned variable, remove this
        buildBinaryOperator t (getBinaryOperator t branch.type_) { compare with left = assign.right }
    | _ -> failwith "Instruction is not a valid statement"

let private buildStatement (t: T) (instr: ILInstruction): Statement =
    match instr with
    | Add binary ->
        Assignment (buildVar t binary.left binary.leftId, buildBinaryOperator t Operator.Add binary)
    | And binary ->
        Assignment (buildVar t binary.left binary.leftId, buildBinaryOperator t Operator.And binary)
    | Assign binary ->
        Assignment (buildVar t binary.left binary.leftId, buildExpression t binary.right binary.rightId)
    | Call unary ->
        FunctionCall (buildExpression t unary.operand unary.operandId)
    | Divide binary ->
        Assignment (buildVar t binary.left binary.leftId, buildBinaryOperator t Operator.Divide binary)
    | Multiply binary ->
        Assignment (buildVar t binary.left binary.leftId, buildBinaryOperator t Operator.Multiply binary)
    | Negate unary ->
        Assignment (buildVar t unary.operand unary.operandId, buildUnaryOperator t Operator.Negate unary)
    | Not unary ->
        Assignment (buildVar t unary.operand unary.operandId, buildUnaryOperator t Operator.Not unary)
    | Or binary ->
        Assignment (buildVar t binary.left binary.leftId, buildBinaryOperator t Operator.Or binary)
    | Return unary ->
        Statement.Return (if unary.operandId = -1 then Option.None else Some(buildVar t unary.operand unary.operandId))
    | ShiftLeft binary ->
        Assignment (buildVar t binary.left binary.leftId, buildBinaryOperator t Operator.ShiftLeft binary)
    | ShiftRight binary ->
        Assignment (buildVar t binary.left binary.leftId, buildBinaryOperator t Operator.ShiftRight binary)
    | Subtract binary ->
        Assignment (buildVar t binary.left binary.leftId, buildBinaryOperator t Operator.Subtract binary)
    | Xor binary ->
        Assignment (buildVar t binary.left binary.leftId, buildBinaryOperator t Operator.Xor binary)
    | _ -> failwith "Instruction is not a valid statement"

let private buildExpression (t: T) (op: ILOperand) (id: int): Expression =
    match op with
    | Pointer _ -> Dereference (VarRef (Var (getVarName t id)))
    | Register _ -> VarRef (Var (getVarName t id))
    | Stack offset -> VarRef (Var (if offset >= 0 then t.parameterNames.[offset] else t.localNames.[offset]))
    | Value value -> Expression.Value (value)
    | NoOperand -> impossible

let private getBinaryOperator (t: T) (condition: BranchType): Operator =
    match condition with
    | Equal -> Operator.Equal
    | NotEqual -> Operator.NotEqual
    | Less -> Operator.Less
    | LessOrEqual -> Operator.LessOrEqual
    | GreaterOrEqual -> Operator.GreaterOrEqual
    | Greater -> Operator.Greater
    | Unconditional -> impossible

let private buildBinaryOperator (t: T) (op: Operator) (instr: BinaryInstruction): Expression =
    Binary (op, buildExpression t instr.left instr.leftId, buildExpression t instr.right instr.rightId)

let private buildUnaryOperator (t: T) (op: Operator) (instr: UnaryInstruction): Expression =
    Unary (op, buildExpression t instr.operand instr.operandId)

let private buildVar (t: T) (op: ILOperand) (id: int): Var =
    match op with
    | Register _ -> Var (getVarName t id)
    | Stack offset -> Var (if offset >= 0 then t.parameterNames.[offset] else t.localNames.[offset])
    | _ -> notSupported

let private getVarName (t: T) (id: int): string =
    if id = -1 then
        failwith "Invalid id"
    else
        let hasValue, name = t.variableNames.TryGetValue(id)
        if hasValue then
            name
        else
            let name = "var" + string(t.nextVariableNameIdx)
            t.nextVariableNameIdx <- t.nextVariableNameIdx + 1
            t.variableNames.[id] <- name
            t.types.[name] <- t.variableTypes.[id]
            name
