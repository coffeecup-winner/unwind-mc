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
        | Break -> None
        | DoWhile (loop, _) -> loop |> Seq.tryPick findRoot
        | For (_, modifier, body) -> modifier |> Seq.append body |> Seq.tryPick findRoot
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
}

let emit (func: Function): string =
    let t = {
        func = func
        parameterNames = new HashSet<string>()
        types = new Dictionary<string, DataType>()
    }
    for param in func.parameters do
        t.parameterNames.Add(param.name) |> ignore
        t.types.Add(param.name, param.type_)
    for var in func.locals |> Seq.append func.variables do
        t.types.Add(var.name, var.type_)
    text {
        yield! emitSignature t (findRootVar t.func.body)
        yield! emitBody t
    } |> buildText { indentSize = 2 }

let private emitSignature (t: T) (ret: Var option): TextWorkflow.T =
    text {
        yield! emitReturnType t ret
        yield t.func.name
        yield "("
        for i in [0 .. t.func.parameters.Count - 1] do
            if i <> 0 then
                yield ", "
            yield! emitDeclaration t.func.parameters.[i].name t.func.parameters.[i].type_
        yield ")"
        yield NewLine
    }

let private emitReturnType (t: T) (var: Var option): TextWorkflow.T =
    text {
        match var with
        | None ->
            yield "void "
        | Some (Var name) ->
            let type_ = t.types.[name]
            match type_ with
            | Function ->
                FIXME "emitting functions not supported yet"
            | _ -> ()
            yield "int "
            yield! emitPointerStars type_
    }

let emitPointerStars (type_: DataType): TextWorkflow.T =
    text {
        match type_ with
        | Pointer t ->
            yield "*"
            yield! emitPointerStars t
        | _ -> ()
    }

let private emitBody (t: T): TextWorkflow.T =
    text {
        yield "{"
        yield NewLine
        yield IncreaseIndent

        yield! emitDeclarations t.func.locals t.func.variables
        for node in t.func.body do
            yield! emitStatement t node

        yield DecreaseIndent
        yield "}"
        yield NewLine
    }

let private emitDeclarations (locals: IReadOnlyList<Variable>) (variables: IReadOnlyList<Variable>): TextWorkflow.T =
    text {
        for var in Seq.append locals variables do
            yield! emitDeclaration var.name var.type_
            yield ";"
            yield NewLine
        yield NewLine
    }

let private emitDeclaration (name: string) (type_: DataType): TextWorkflow.T =
    text {
        if isFunction type_ then
            yield "void"
            yield " "
            yield "("
            yield "*"
            yield! emitPointerStars type_
            yield name
            yield ")"
            yield "()"
        else
            yield "int "
            yield! emitPointerStars type_
            yield name
    }

let private emitStatement (t: T) (statement: Statement): TextWorkflow.T =
    text {
        yield!
            match statement with
            | Assignment (var, expr) -> emitAssignment t var expr true
            | Break -> emitBreak ()
            | DoWhile (loop, cond) -> emitDoWhile t loop cond
            | For (cond, modifier, body) -> emitFor t cond modifier body
            | FunctionCall expr -> emitFunctionCall t expr
            | IfThenElse (cond, trueBranch, falseBranch) -> emitIfThenElse t cond trueBranch falseBranch
            | Return var -> emitReturn var
            | While (cond, loop) -> emitWhile t cond loop
    }

let private emitAssignment (t :T) (var: Var) (expr: Expression) (emitSeparator: bool): TextWorkflow.T =
    text {
        let (Var name) = var
        yield name
        yield " = "
        yield! emitExpression t expr
        if emitSeparator then
            yield ";"
            yield NewLine
    }

let private emitBreak (): TextWorkflow.T =
    text {
        yield "break"
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

let private emitFor (t: T) (cond: Expression) (modifier: IReadOnlyList<Statement>) (body: IReadOnlyList<Statement>): TextWorkflow.T =
    text {
        yield "for"
        yield " "
        yield "("
        yield ";"
        yield " "
        yield! emitExpression t cond
        yield ";"
        yield " "
        for (index, stmt) in modifier |> Seq.indexed do
            match stmt with
            | Assignment (var, expr) -> yield! emitAssignment t var expr false
            | _ -> impossible
            if index < modifier.Count - 1 then
                yield ","
                yield " "
        yield ")"
        yield NewLine

        yield! emitScope t body false
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
