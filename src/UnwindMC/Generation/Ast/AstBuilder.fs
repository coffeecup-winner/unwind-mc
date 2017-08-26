module rec AstBuilder

open System
open System.Collections.Generic
open UnwindMC.Analysis.IL
open Ast
open Type
open FlowAnalyzer

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

let buildAst (blocks: IReadOnlyList<Block>) (parameterTypes: IReadOnlyList<DataType>) (localTypes: IReadOnlyList<DataType>) (variableTypes: IReadOnlyList<DataType>): Statement =
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
    let ast = buildScope t t.blocks
    runTransformations t ast

let private runTransformations (t: T) (ast: Statement): Statement =
    [
        FixupPointerArithmetics.transform t.types
        FixupZeroAssignment.transform
    ] |> Seq.fold (fun a t -> t a) ast

let private buildScope (t: T) (blocks: IReadOnlyList<Block>): Statement =
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
    Scope statements

let private buildWhile (t: T) (condition: ILInstruction) (children: IReadOnlyList<Block>): Statement =
    While (buildCondition t condition, buildScope t children)

let private buildDoWhile (t: T) (condition: ILInstruction) (children: IReadOnlyList<Block>): Statement =
    DoWhile (buildScope t children, buildCondition t condition)

let private buildIfThenElse (t: T) (condition: ILInstruction) (trueBranch: IReadOnlyList<Block>) (falseBranch: IReadOnlyList<Block>): Statement =
    IfThenElse (buildCondition t condition, buildScope t trueBranch, buildScope t falseBranch)

let private buildCondition (t: T) (instr: ILInstruction): Expression =
    match instr.Type with
    | ILInstructionType.Compare ->
        buildBinaryOperator t (getBinaryOperator t instr.Condition) instr
    | _ -> raise (new ArgumentException("Instruction is not a valid statement"))

let private buildStatement (t: T) (instr: ILInstruction): Statement =
    match instr.Type with
    | ILInstructionType.Add ->
        Assignment (buildVar t instr.Target instr.TargetId, buildBinaryOperator t Operator.Add instr)
    | ILInstructionType.And ->
        Assignment (buildVar t instr.Target instr.TargetId, buildBinaryOperator t Operator.And instr)
    | ILInstructionType.Assign ->
        Assignment (buildVar t instr.Target instr.TargetId, buildExpression t instr.Source instr.SourceId)
    | ILInstructionType.Call ->
        FunctionCall (buildExpression t instr.Target instr.TargetId)
    | ILInstructionType.Divide ->
        Assignment (buildVar t instr.Target instr.TargetId, buildBinaryOperator t Operator.Divide instr)
    | ILInstructionType.Multiply ->
        Assignment (buildVar t instr.Target instr.TargetId, buildBinaryOperator t Operator.Multiply instr)
    | ILInstructionType.Negate ->
        Assignment (buildVar t instr.Target instr.TargetId, buildUnaryOperator t Operator.Negate instr)
    | ILInstructionType.Not ->
        Assignment (buildVar t instr.Target instr.TargetId, buildUnaryOperator t Operator.Not instr)
    | ILInstructionType.Or ->
        Assignment (buildVar t instr.Target instr.TargetId, buildBinaryOperator t Operator.Or instr)
    | ILInstructionType.Return ->
        Return (if instr.SourceId = -1 then None else Some(buildVar t instr.Source instr.SourceId))
    | ILInstructionType.ShiftLeft ->
        Assignment (buildVar t instr.Target instr.TargetId, buildBinaryOperator t Operator.ShiftLeft instr)
    | ILInstructionType.ShiftRight ->
        Assignment (buildVar t instr.Target instr.TargetId, buildBinaryOperator t Operator.ShiftRight instr)
    | ILInstructionType.Subtract ->
        Assignment (buildVar t instr.Target instr.TargetId, buildBinaryOperator t Operator.Subtract instr)
    | ILInstructionType.Xor ->
        Assignment (buildVar t instr.Target instr.TargetId, buildBinaryOperator t Operator.Xor instr)
    | _ -> raise (new ArgumentException("Instruction is not a valid statement"))

let private buildExpression (t: T) (op: ILOperand) (id: int): Expression =
    match op.Type with
    | ILOperandType.Pointer -> Dereference (VarRef (Var (getVarName t id)))
    | ILOperandType.Register -> VarRef (Var (getVarName t id))
    | ILOperandType.Stack -> VarRef (Var (if op.Offset >= 0 then t.parameterNames.[op.Offset] else t.localNames.[op.Offset]))
    | ILOperandType.Value -> Value (op.Value)
    | _ -> raise (new InvalidOperationException())

let private getBinaryOperator (t: T) (condition: ILBranchType): Operator =
    match condition with
    | ILBranchType.Equal -> Operator.Equal
    | ILBranchType.NotEqual -> Operator.NotEqual
    | ILBranchType.Less -> Operator.Less
    | ILBranchType.LessOrEqual -> Operator.GreaterOrEqual
    | ILBranchType.GreaterOrEqual -> Operator.GreaterOrEqual
    | ILBranchType.Greater -> Operator.Greater
    | ILBranchType.Next -> raise (new InvalidOperationException("Next is not a valid operator"))
    | _ -> raise (new InvalidOperationException())

let private buildBinaryOperator (t: T) (op: Operator) (instr: ILInstruction): Expression =
    Binary (op, buildExpression t instr.Target instr.TargetId, buildExpression t instr.Source instr.SourceId)

let private buildUnaryOperator (t: T) (op: Operator) (instr: ILInstruction): Expression =
    Unary (op, buildExpression t instr.Target instr.TargetId)

let private buildVar (t: T) (op: ILOperand) (id: int): Var =
    match op.Type with
    | ILOperandType.Register -> Var (getVarName t id)
    | ILOperandType.Stack -> Var (if op.Offset >= 0 then t.parameterNames.[op.Offset] else t.localNames.[op.Offset])
    | _ -> raise (new NotSupportedException())

let private getVarName (t: T) (id: int): string =
    if id = -1 then
        raise (new InvalidOperationException("Invalid id"))
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
