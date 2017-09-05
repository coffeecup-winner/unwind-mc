module rec CppEmitter

open System
open System.Collections.Generic
open Ast
open TextWorkflow
open Type
open AstBuilder

let private findRootVar (stmts: IReadOnlyList<Statement>): Var option =
    let rec findRoot =
        function
        | Assignment _ -> None
        | DoWhile (loop, _) -> loop |> Seq.tryPick findRoot
        | FunctionCall _ -> None
        | IfThenElse (_, trueBranch, falseBranch) ->
            let root = trueBranch |> Seq.tryPick findRoot
            if root.IsSome then root else falseBranch |> Seq.tryPick findRoot
        | Return var -> var
        | While (_, loop) -> loop |> Seq.tryPick findRoot
    stmts
    |> Seq.rev
    |> Seq.tryPick findRoot

type private T = {
    func: Function
    parameterNames: HashSet<string>
    types: Dictionary<string, DataType>
    declaredVariables: HashSet<string>
}

let emit (func: Function): string =
    let t = {
        func = func
        parameterNames = new HashSet<string>()
        types = new Dictionary<string, DataType>()
        declaredVariables = new HashSet<string>()
    }
    for param in func.parameters do
        t.parameterNames.Add(param.name) |> ignore
        t.types.Add(param.name, param.type_)
    for var in func.locals |> Seq.append func.variables do
        t.types.Add(var.name, var.type_)
    text {
        yield! emitSignature t (findRootVar t.func.body)
        yield! emitScope t t.func.body false
    } |> buildText { indentSize = 2 }

let private emitSignature (t: T) (ret: Var option): TextWorkflow.T =
    text {
        yield! emitType t ret
        yield t.func.name
        yield "("
        for i in [0 .. t.func.parameters.Count - 1] do
            if i <> 0 then
                yield ", "
            yield! emitDeclaration t.func.parameters.[i].name t.func.parameters.[i].type_
        yield ")"
        yield NewLine
    }

let private emitType (t: T) (var: Var option): TextWorkflow.T =
    text {
        match var with
        | None ->
            yield "void "
        | Some (Var name) ->
            let type_ = t.types.[name]
            if type_.isFunction then
                FIXME "emitting functions not supported yet"
            yield "int "
            yield new System.String('*', type_.indirectionLevel)
    }

let private emitDeclaration (name: string) (type_: DataType): TextWorkflow.T =
    text {
        if type_.isFunction then
            yield "void"
            yield " "
            yield "("
            yield new System.String('*', type_.indirectionLevel + 1)
            yield name
            yield ")"
            yield "()"
        else
            yield "int "
            yield new System.String('*', type_.indirectionLevel)
            yield name
    }

let private emitStatement (t: T) (statement: Statement): TextWorkflow.T =
    text {
        yield!
            match statement with
            | Assignment (var, expr) -> emitAssignment t var expr
            | DoWhile (loop, cond) -> emitDoWhile t loop cond
            | FunctionCall expr -> emitFunctionCall t expr
            | IfThenElse (cond, trueBranch, falseBranch) -> emitIfThenElse t cond trueBranch falseBranch
            | Return var -> emitReturn var
            | While (cond, loop) -> emitWhile t cond loop
    }

let private emitAssignment (t :T) (var: Var) (expr: Expression): TextWorkflow.T =
    text {
        let (Var name) = var
        if not (t.parameterNames.Contains(name)) && t.declaredVariables.Add(name) then
            yield! emitDeclaration name t.types.[name]
        else
            yield name
        yield " = "
        yield! emitExpression t expr
        yield ";"
        yield NewLine
    }

let private emitDoWhile (t: T) (loop: IReadOnlyList<Statement>) (cond: Expression): TextWorkflow.T =
    text {
        yield "do"
        yield NewLine

        yield! emitScope t loop true
        yield " "
        yield "while"
        yield " "
        yield "("
        yield! emitExpression t cond
        yield ")"
        yield ";"
        yield NewLine
    }

let private emitFunctionCall (t: T) (expr: Expression): TextWorkflow.T =
    text {
        yield! emitExpression t expr
        yield "()"
        yield ";"
        yield NewLine
    }

let private emitIfThenElse (t: T) (cond: Expression) (trueBranch: IReadOnlyList<Statement>) (falseBranch: IReadOnlyList<Statement>): TextWorkflow.T =
    text {
        yield "if"
        yield " "
        yield "("
        yield! emitExpression t cond
        yield ")"
        yield NewLine

        yield! emitScope t trueBranch false
        if falseBranch.Count > 0 then
            yield "else"
            yield NewLine
            yield! emitScope t falseBranch false
    }

let private emitReturn (var: Var option): TextWorkflow.T =
    text {
        yield "return"
        match var with
        | Some (Var name) ->
            yield " "
            yield name
        | _ -> ()
        yield ";"
        yield NewLine
    }

let private emitScope (t: T) (stmts: IReadOnlyList<Statement>) (skipNewline: bool): TextWorkflow.T =
    text {
        yield "{"
        yield NewLine

        yield IncreaseIndent
        for node in stmts do
            yield! emitStatement t node
        yield DecreaseIndent

        yield "}"
        if not skipNewline then
            yield NewLine
    }

let private emitWhile (t: T) (cond: Expression) (loop: IReadOnlyList<Statement>): TextWorkflow.T =
    text {
        yield "while"
        yield " "
        yield "("
        yield! emitExpression t cond
        yield ")"
        yield NewLine

        yield! emitScope t loop false
    }

let private emitExpression (t: T) (expression: Expression): TextWorkflow.T =
    text {
        yield!
            match expression with
            | Binary (op, left, right) -> emitBinary t op left right
            | Dereference expr -> emitDereference t expr
            | Unary (op, operand) -> emitUnary t op operand
            | Value value -> emitValue value
            | VarRef var -> emitVar var
    }

let private emitBinary (t: T) (op: Operator) (left: Expression) (right: Expression): TextWorkflow.T =
    text {
        yield! emitExpression t left
        yield " "
        yield
            match op with
            | Operator.Equal -> "=="
            | Operator.NotEqual -> "!="
            | Operator.Less -> "<"
            | Operator.LessOrEqual -> "<="
            | Operator.Greater -> ">"
            | Operator.GreaterOrEqual -> ">="
            | Operator.And -> "&"
            | Operator.Or -> "|"
            | Operator.Xor -> "^"
            | Operator.ShiftLeft -> "<<"
            | Operator.ShiftRight -> ">>"
            | Operator.Add -> "+"
            | Operator.Subtract -> "-"
            | Operator.Multiply -> "*"
            | Operator.Divide -> "/"
            | Operator.Modulo -> "%"
            | _ -> notSupported
        yield " "
        yield! emitExpression t right
    }

let private emitDereference (t: T) (expr: Expression): TextWorkflow.T =
    text {
        yield "*"
        yield "("
        yield! emitExpression t expr
        yield ")"
    }

let private emitUnary (t: T) (op: Operator) (operand: Expression): TextWorkflow.T =
    text {
        yield
            match op with
            | Operator.Negate -> "-"
            | Operator.Not -> "~"
            | _ -> notSupported
        yield! emitExpression t operand
    }

let private emitValue (value: int): TextWorkflow.T =
    text {
        yield value.ToString()
    }

let private emitVar (var: Var): TextWorkflow.T =
    text {
        let (Var name) = var
        yield name
    }
