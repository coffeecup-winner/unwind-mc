﻿module rec CppEmitter

open System
open System.Collections.Generic
open Ast
open TextWorkflow
open Type

let private findRootVar (stmts: IReadOnlyList<Statement>): Var option =
    let tryFind (mapping: 'a -> 'b option): 'a seq -> 'b option =
        Seq.map mapping
        >> Seq.tryFind (fun o -> o.IsSome)
        >> Option.flatten
    let rec findRoot =
        function
        | Assignment _ -> None
        | DoWhile (loop, _) -> loop |> tryFind findRoot
        | FunctionCall _ -> None
        | IfThenElse (_, trueBranch, falseBranch) ->
            let root = trueBranch |> tryFind findRoot
            if root.IsSome then root else falseBranch |> tryFind findRoot
        | Return var -> var
        | While (_, loop) -> loop |> tryFind findRoot
    stmts
    |> Seq.rev
    |> tryFind findRoot

[<Literal>]
let IndentSize = 2

type private T = {
    name: string
    types: IReadOnlyDictionary<string, DataType>
    parametersCount: int
    body: IReadOnlyList<Statement>
    declaredVariables: HashSet<string>
}

let emit (name: string) (types: IReadOnlyDictionary<string, DataType>) (parametersCount: int) (body: IReadOnlyList<Statement>): string =
    let t = {
        name = name
        types = types
        parametersCount = parametersCount
        body = body
        declaredVariables = new HashSet<string>()
    }
    text {
        yield! emitSignature t (findRootVar t.body)
        yield! emitScope t t.body false
    } |> buildText { indentSize = 2 }

let private emitSignature (t: T) (ret: Var option): TextWorkflow.T =
    text {
        yield! emitType t ret
        yield t.name
        yield "("
        for i in [0 .. t.parametersCount - 1] do
            if i <> 0 then
                yield ", "
            yield! emitDeclaration t ("arg" + string(i)) // TODO: move argument names to function
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
                raise (new NotImplementedException())
            yield "int "
            yield new System.String('*', type_.indirectionLevel)
    }

let private emitDeclaration (t: T) (name: string): TextWorkflow.T =
    text {
        let type_ = t.types.[name]
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
            | Return var -> emitReturn t var
            | While (cond, loop) -> emitWhile t cond loop
    }

let private emitAssignment (t :T) (var: Var) (expr: Expression): TextWorkflow.T =
    text {
        let (Var name) = var
        yield Indent
        // TODO: rework how variables are passed and treated in each step
        if not (name.StartsWith("arg")) && t.declaredVariables.Add(name) then
            yield! emitDeclaration t name
        else
            yield name
        yield " = "
        yield! emitExpression t expr
        yield ";"
        yield NewLine
    }

let private emitDoWhile (t: T) (loop: IReadOnlyList<Statement>) (cond: Expression): TextWorkflow.T =
    text {
        yield Indent
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
        yield Indent
        yield! emitExpression t expr
        yield "()"
        yield ";"
        yield NewLine
    }

let private emitIfThenElse (t: T) (cond: Expression) (trueBranch: IReadOnlyList<Statement>) (falseBranch: IReadOnlyList<Statement>): TextWorkflow.T =
    text {
        yield Indent
        yield "if"
        yield " "
        yield "("
        yield! emitExpression t cond
        yield ")"
        yield NewLine

        yield! emitScope t trueBranch false
        if falseBranch.Count > 0 then
            yield Indent
            yield "else"
            yield NewLine
            yield! emitScope t falseBranch false
    }

let private emitReturn (t: T) (var: Var option): TextWorkflow.T =
    text {
        yield Indent
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
        yield Indent
        yield "{"
        yield NewLine

        yield IncreaseIndent
        for node in stmts do
            yield! emitStatement t node
        yield DecreaseIndent

        yield Indent
        yield "}"
        if not skipNewline then
            yield NewLine
    }

let private emitWhile (t: T) (cond: Expression) (loop: IReadOnlyList<Statement>): TextWorkflow.T =
    text {
        yield Indent
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
            | Value value -> emitValue t value
            | VarRef var -> emitVar t var
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
            | _ -> raise (new NotSupportedException())
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
            | _ -> raise (new NotSupportedException())
        yield! emitExpression t operand
    }

let private emitValue (t: T) (value: int): TextWorkflow.T =
    text {
        yield value.ToString()
    }

let private emitVar (t: T) (var: Var): TextWorkflow.T =
    text {
        let (Var name) = var
        yield name
    }
