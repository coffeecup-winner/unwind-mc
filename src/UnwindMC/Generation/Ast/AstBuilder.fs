module rec AstBuilder

open System
open System.Collections.Generic
open UnwindMC.Analysis
open UnwindMC.Analysis.Flow
open UnwindMC.Analysis.IL
open Ast

type private T = {
    blocks: IReadOnlyList<IBlock>
    parameterTypes: IReadOnlyList<Data.Type>
    localTypes: IReadOnlyList<Data.Type>
    variableTypes: IReadOnlyList<Data.Type>
    variableNames: Dictionary<int, string>
    parameterNames: Dictionary<int, string>
    localNames: Dictionary<int, string>
    types: Dictionary<string, Data.Type>
    mutable nextVariableNameIdx: int
}

let buildAst (blocks: IReadOnlyList<IBlock>) (parameterTypes: IReadOnlyList<Data.Type>) (localTypes: IReadOnlyList<Data.Type>) (variableTypes: IReadOnlyList<Data.Type>): Statement =
    let t = {
        blocks = blocks
        parameterTypes = parameterTypes
        localTypes = localTypes
        variableTypes = variableTypes
        variableNames = new Dictionary<int, string>()
        parameterNames = new Dictionary<int, string>()
        localNames = new Dictionary<int, string>()
        types = new Dictionary<string, Data.Type>()
        nextVariableNameIdx = 0
    }
    let mutable offset = 0
    for index in [0 .. t.parameterTypes.Count - 1] do
        let name = "arg" + string(index)
        t.parameterNames.[offset] <- name;
        t.types.[name] <- t.parameterTypes.[index]
        offset <- offset + t.parameterTypes.[index].Size
    offset <- -8; // TODO: this will not always be correct
    for index in [0 .. t.localTypes.Count - 1] do
        offset <- offset - t.localTypes.[index].Size
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

let private buildScope (t: T) (blocks: IReadOnlyList<IBlock>): Statement =
    let statements = new List<Statement>()
    for block in blocks do
        match block with
        | :? SequentialBlock as seq ->
            for instr in seq.Instructions do
                statements.Add(buildStatement t instr);
        | :? WhileBlock as whileLoop ->
            statements.Add(buildWhile t whileLoop);
        | :? DoWhileBlock as doWhileLoop ->
            statements.Add(buildDoWhile t doWhileLoop);
        | :? ConditionalBlock as cond ->
            statements.Add(buildIfThenElse t cond);
        | _ -> raise (new NotSupportedException())
    Scope statements

let private buildWhile (t: T) (loop: WhileBlock): Statement =
    While (buildCondition t loop.Condition, buildScope t loop.Children)

let private buildDoWhile (t: T) (doWhileLoop: DoWhileBlock): Statement =
    DoWhile (buildScope t doWhileLoop.Children, buildCondition t doWhileLoop.Condition)

let private buildIfThenElse (t: T) (cond: ConditionalBlock): Statement =
    IfThenElse (buildCondition t cond.Condition, buildScope t cond.TrueBranch, buildScope t cond.FalseBranch)

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
