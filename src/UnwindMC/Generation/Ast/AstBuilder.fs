module rec AstBuilder

open System.Collections.Generic
open Ast
open IL
open FlowAnalyzer
open Type

type private T = {
    blocks: IReadOnlyList<Block>
    parameterTypes: IReadOnlyList<DataType>
    localTypes: IReadOnlyList<DataType>
    variableTypes: IReadOnlyList<DataType>
    variableNames: Dictionary<int, string>
    parameterNames: Dictionary<int, string>
    localNames: Dictionary<int, string>
    types: Dictionary<string, DataType>
    mutable nextVariableNameIdx: int
}

let buildAst (blocks: IReadOnlyList<Block>) (parameterTypes: IReadOnlyList<DataType>) (localTypes: IReadOnlyList<DataType>) (variableTypes: IReadOnlyList<DataType>): IReadOnlyList<Statement> =
    let t = {
        blocks = blocks
        parameterTypes = parameterTypes
        localTypes = localTypes
        variableTypes = variableTypes
        variableNames = new Dictionary<int, string>()
        parameterNames = new Dictionary<int, string>()
        localNames = new Dictionary<int, string>()
        types = new Dictionary<string, DataType>()
        nextVariableNameIdx = 0
    }
    let mutable offset = 0
    for index in [0 .. t.parameterTypes.Count - 1] do
        let name = "arg" + string(index)
        t.parameterNames.[offset] <- name;
        t.types.[name] <- t.parameterTypes.[index]
        offset <- offset + t.parameterTypes.[index].size
    offset <- -8; // TODO: this will not always be correct
    for index in [0 .. t.localTypes.Count - 1] do
        offset <- offset - t.localTypes.[index].size
        let name = "loc" + string(index)
        t.localNames.[offset] <- name
        t.types.[name] <- t.localTypes.[index]
    buildScope t t.blocks
    |> ROL.map (runTransformations t)

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
            for instr in is do
                statements.Add(buildStatement t instr);
        | WhileBlock { condition = c; children = cs } ->
            statements.Add(buildWhile t c cs);
        | DoWhileBlock { condition = c; children = cs } ->
            statements.Add(buildDoWhile t c cs);
        | ConditionalBlock { condition = c; trueBranch = tb; falseBranch = fb } ->
            statements.Add(buildIfThenElse t c tb fb);
    statements :> IReadOnlyList<Statement>

let private buildWhile (t: T) (condition: ILInstruction) (children: IReadOnlyList<Block>): Statement =
    While (buildCondition t condition, buildScope t children)

let private buildDoWhile (t: T) (condition: ILInstruction) (children: IReadOnlyList<Block>): Statement =
    DoWhile (buildScope t children, buildCondition t condition)

let private buildIfThenElse (t: T) (condition: ILInstruction) (trueBranch: IReadOnlyList<Block>) (falseBranch: IReadOnlyList<Block>): Statement =
    IfThenElse (buildCondition t condition, buildScope t trueBranch, buildScope t falseBranch)

let private buildCondition (t: T) (instr: ILInstruction): Expression =
    match instr.type_ with
    | Compare ->
        buildBinaryOperator t (getBinaryOperator t instr.condition) instr
    | _ -> failwith "Instruction is not a valid statement"

let private buildStatement (t: T) (instr: ILInstruction): Statement =
    match instr.type_ with
    | Add ->
        Assignment (buildVar t instr.target instr.targetId, buildBinaryOperator t Operator.Add instr)
    | And ->
        Assignment (buildVar t instr.target instr.targetId, buildBinaryOperator t Operator.And instr)
    | Assign ->
        Assignment (buildVar t instr.target instr.targetId, buildExpression t instr.source instr.sourceId)
    | Call ->
        FunctionCall (buildExpression t instr.target instr.targetId)
    | Divide ->
        Assignment (buildVar t instr.target instr.targetId, buildBinaryOperator t Operator.Divide instr)
    | Multiply ->
        Assignment (buildVar t instr.target instr.targetId, buildBinaryOperator t Operator.Multiply instr)
    | Negate ->
        Assignment (buildVar t instr.target instr.targetId, buildUnaryOperator t Operator.Negate instr)
    | Not ->
        Assignment (buildVar t instr.target instr.targetId, buildUnaryOperator t Operator.Not instr)
    | Or ->
        Assignment (buildVar t instr.target instr.targetId, buildBinaryOperator t Operator.Or instr)
    | Return ->
        Statement.Return (if instr.sourceId = -1 then Option.None else Some(buildVar t instr.source instr.sourceId))
    | ShiftLeft ->
        Assignment (buildVar t instr.target instr.targetId, buildBinaryOperator t Operator.ShiftLeft instr)
    | ShiftRight ->
        Assignment (buildVar t instr.target instr.targetId, buildBinaryOperator t Operator.ShiftRight instr)
    | Subtract ->
        Assignment (buildVar t instr.target instr.targetId, buildBinaryOperator t Operator.Subtract instr)
    | Xor ->
        Assignment (buildVar t instr.target instr.targetId, buildBinaryOperator t Operator.Xor instr)
    | _ -> failwith "Instruction is not a valid statement"

let private buildExpression (t: T) (op: ILOperand) (id: int): Expression =
    match op with
    | Pointer _ -> Dereference (VarRef (Var (getVarName t id)))
    | Register _ -> VarRef (Var (getVarName t id))
    | Stack offset -> VarRef (Var (if offset >= 0 then t.parameterNames.[offset] else t.localNames.[offset]))
    | Value value -> Expression.Value (value)
    | NoOperand -> impossible

let private getBinaryOperator (t: T) (condition: ILBranchType): Operator =
    match condition with
    | Equal -> Operator.Equal
    | NotEqual -> Operator.NotEqual
    | Less -> Operator.Less
    | LessOrEqual -> Operator.GreaterOrEqual
    | GreaterOrEqual -> Operator.GreaterOrEqual
    | Greater -> Operator.Greater
    | Next -> failwith "Next is not a valid operator"

let private buildBinaryOperator (t: T) (op: Operator) (instr: ILInstruction): Expression =
    Binary (op, buildExpression t instr.target instr.targetId, buildExpression t instr.source instr.sourceId)

let private buildUnaryOperator (t: T) (op: Operator) (instr: ILInstruction): Expression =
    Unary (op, buildExpression t instr.target instr.targetId)

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
