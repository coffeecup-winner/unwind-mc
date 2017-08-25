module rec CppEmitter

open System
open System.Collections.Generic
open System.Text
open Ast

let private findRootVar (stmt: Statement): Var option =
    match stmt with
    | Assignment _ -> None
    | DoWhile (loop, _) -> findRootVar loop
    | FunctionCall _ -> None
    | IfThenElse (_, trueBranch, falseBranch) ->
        let root = findRootVar trueBranch
        if root.IsSome then root else findRootVar falseBranch
    | Return var -> var
    | Scope stmts -> stmts |> Seq.fold (fun o s -> if o.IsSome then o else findRootVar s) None
    | While (_, loop) -> findRootVar loop

[<Literal>]
let IndentSize = 2

type private T = {
    sb: StringBuilder
    name: string
    types: IReadOnlyDictionary<string, UnwindMC.Analysis.Data.Type>
    parametersCount: int
    body: Statement
    declaredVariables: HashSet<string>
    indents: Dictionary<int, string>
    mutable indentLevel: int
    mutable indent: string
}

let emit (name: string) (types: IReadOnlyDictionary<string, UnwindMC.Analysis.Data.Type>) (parametersCount: int) (body: Statement): string =
    let t = {
        sb = new StringBuilder()
        name = name
        types = types
        parametersCount = parametersCount
        body = body
        declaredVariables = new HashSet<string>()
        indents = new Dictionary<int, string>()
        indentLevel = 0
        indent = ""
    }
    t.indents.Add(0, "")
    emitSourceCode t

let private emitSourceCode (t: T): string =
    emitSignature t (findRootVar t.body)
    let (Scope stmts) = t.body
    emitScope t stmts false
    t.sb.ToString()

let private emitSignature (t: T) (ret: Var option): unit =
    emitType t ret
    t.sb.Append(t.name)
        .Append("(") |> ignore
    for i in [0 .. t.parametersCount - 1] do
        if i <> 0 then
            t.sb.Append(", ") |> ignore
        emitDeclaration t ("arg" + string(i)) // TODO: move argument names to function
    t.sb.Append(")")
        .Append(Environment.NewLine) |> ignore

let private emitType (t: T) (var: Var option): unit =
    match var with
    | None -> t.sb.Append("void ") |> ignore
    | Some (Var name) ->
        let type_ = t.types.[name]
        if type_.IsFunction then
            raise (new NotImplementedException())
        else
            t.sb.Append("int ")
                .Append(new System.String('*', type_.IndirectionLevel)) |> ignore

let private emitDeclaration (t: T) (name: string): unit =
    let type_ = t.types.[name];
    if type_.IsFunction then
        t.sb.Append("void")
            .Append(" ")
            .Append("(")
            .Append(new System.String('*', type_.IndirectionLevel + 1))
            .Append(name)
            .Append(")")
            .Append("()") |> ignore
    else
        t.sb.Append("int ")
            .Append(new System.String('*', type_.IndirectionLevel))
            .Append(name) |> ignore

let private emitStatement (t: T) (statement: Statement): unit =
    match statement with
    | Assignment (var, expr) -> emitAssignment t var expr
    | DoWhile (loop, cond) -> emitDoWhile t loop cond
    | FunctionCall expr -> emitFunctionCall t expr
    | IfThenElse (cond, trueBranch, falseBranch) -> emitIfThenElse t cond trueBranch falseBranch
    | Return var -> emitReturn t var
    | Scope stmts -> emitScope t stmts false
    | While (cond, loop) -> emitWhile t cond loop

let private emitAssignment (t :T) (var: Var) (expr: Expression): unit =
    let (Var name) = var
    t.sb.Append(t.indent) |> ignore
    // TODO: reword how variables are passed and treated in each step
    if not (name.StartsWith("arg")) && t.declaredVariables.Add(name) then
        emitDeclaration t name
    else
        t.sb.Append(name) |> ignore
    t.sb.Append(" = ") |> ignore
    emitExpression t expr
    t.sb.Append(";")
        .Append(Environment.NewLine) |> ignore

let private emitDoWhile (t: T) (loop: Statement) (cond: Expression): unit =
    t.sb.Append(t.indent)
        .Append("do")
        .Append(Environment.NewLine) |> ignore
    let (Scope stmts) = loop
    emitScope t stmts true
    t.sb.Append(" ")
        .Append("while")
        .Append(" ")
        .Append("(") |> ignore
    emitExpression t cond
    t.sb.Append(")")
        .Append(";")
        .Append(Environment.NewLine) |> ignore

let private emitFunctionCall (t: T) (expr: Expression): unit =
    t.sb.Append(t.indent) |> ignore
    emitExpression t expr
    t.sb.Append("()")
        .Append(";")
        .Append(Environment.NewLine) |> ignore

let private emitIfThenElse (t: T) (cond: Expression) (trueBranch: Statement) (falseBranch: Statement): unit =
    t.sb.Append(t.indent)
        .Append("if")
        .Append(" ")
        .Append("(") |> ignore
    emitExpression t cond
    t.sb.Append(")")
        .Append(Environment.NewLine) |> ignore
    let (Scope stmts) = trueBranch
    emitScope t stmts false
    let (Scope stmts) = falseBranch
    if stmts.Count > 0 then
        t.sb.Append(t.indent)
            .Append("else")
            .Append(Environment.NewLine) |> ignore
        emitScope t stmts false

let private emitReturn (t: T) (var: Var option): unit =
    t.sb.Append(t.indent)
        .Append("return") |> ignore
    match var with
    | Some (Var name) ->
        t.sb.Append(" ")
            .Append(name) |> ignore
    | _ -> ()
    t.sb.Append(";")
        .Append(Environment.NewLine) |> ignore

let private emitScope (t: T) (stmts: IReadOnlyList<Statement>) (skipNewline: bool): unit =
    t.sb.Append(t.indent)
        .Append("{")
        .Append(Environment.NewLine) |> ignore
    t.indentLevel <- t.indentLevel + 1
    let hasValue, indent = t.indents.TryGetValue(t.indentLevel)
    if hasValue then
        t.indent <- indent
    else
        t.indent <- new System.String(' ', t.indentLevel * IndentSize)
        t.indents.[t.indentLevel] <- t.indent

    for node in stmts do
        emitStatement t node

    t.indentLevel <- t.indentLevel - 1
    t.indent <- t.indents.[t.indentLevel]
    t.sb.Append(t.indent)
        .Append("}") |> ignore
    if not skipNewline then
        t.sb.Append(Environment.NewLine) |> ignore

let private emitWhile (t: T) (cond: Expression) (loop: Statement): unit =
    t.sb.Append(t.indent)
        .Append("while")
        .Append(" ")
        .Append("(") |> ignore
    emitExpression t cond
    t.sb.Append(")")
        .Append(Environment.NewLine) |> ignore
    let (Scope stmts) = loop
    emitScope t stmts false

let private emitExpression (t: T) (expression: Expression): unit =
    match expression with
    | Binary (op, left, right) -> emitBinary t op left right
    | Dereference expr -> emitDereference t expr
    | Unary (op, operand) -> emitUnary t op operand
    | Value value -> emitValue t value
    | VarRef var -> emitVar t var

let private emitBinary (t: T) (op: Operator) (left: Expression) (right: Expression): unit =
    emitExpression t left
    t.sb.Append(" ") |> ignore
    let opString =
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
    t.sb.Append(opString)
        .Append(" ") |> ignore
    emitExpression t right

let private emitDereference (t: T) (expr: Expression): unit =
    t.sb.Append("*")
        .Append("(") |> ignore
    emitExpression t expr
    t.sb.Append(")") |> ignore

let private emitUnary (t: T) (op: Operator) (operand: Expression): unit =
    let opString =
        match op with
        | Operator.Negate -> "-"
        | Operator.Not -> "~"
        | _ -> raise (new NotSupportedException())
    t.sb.Append(opString) |> ignore
    emitExpression t operand

let private emitValue (t: T) (value: int): unit =
    t.sb.Append(value) |> ignore

let private emitVar (t: T) (var: Var): unit =
    let (Var name) = var
    t.sb.Append(name) |> ignore
