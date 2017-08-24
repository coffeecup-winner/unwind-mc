module rec CppEmitter

open System;
open System.Collections.Generic;
open System.Text;
open UnwindMC.Generation.Ast;
open UnwindMC.Util;

type ReturnNodeFinder() =
    inherit NodeTransformerBase()

    let mutable _ret: ReturnNode = null

    member self.ret: ReturnNode = _ret
    override self.Transform(ret: ReturnNode): ReturnNode =
        _ret <- ret
        ret

[<Literal>]
let IndentSize = 2

type T = {
    _sb: StringBuilder
    _name: string
    _types: IReadOnlyDictionary<string, UnwindMC.Analysis.Data.Type>
    _parametersCount: int
    _body: ScopeNode
    _declaredVariables: HashSet<string>
    _indents: Dictionary<int, string>
    mutable _indentLevel: int
    mutable _indent: string
}

let emit (name: string) (types: IReadOnlyDictionary<string, UnwindMC.Analysis.Data.Type>) (parametersCount: int) (body: ScopeNode): string =
    let t = {
        _sb = new StringBuilder()
        _name = name
        _types = types
        _parametersCount = parametersCount
        _body = body
        _declaredVariables = new HashSet<string>()
        _indents = new Dictionary<int, string>()
        _indentLevel = 0
        _indent = ""
    }
    t._indents.Add(0, "")
    emitSourceCode t

let emitSourceCode (t: T): string =
    emitSignature t (t._body.Transform(new ReturnNodeFinder()).ret)
    emitScope t t._body false
    t._sb.ToString()

let emitSignature (t: T) (ret: ReturnNode): unit =
    emitType t ret.Var
    t._sb.Append(t._name)
        .Append("(") |> ignore
    for i in [0 .. t._parametersCount - 1] do
        if i <> 0 then
            t._sb.Append(", ") |> ignore
        emitDeclaration t ("arg" + string(i)) // TODO: move argument names to function
    t._sb.Append(")")
        .Append(Environment.NewLine) |> ignore

let emitType (t: T) (node: Option<VarNode>): unit =
    let isSome, var = node.TryGet()
    if not isSome then
        t._sb.Append("void ") |> ignore
    else
        let type_ = t._types.[var.Name]
        if type_.IsFunction then
            raise (new NotImplementedException())
        else
            t._sb.Append("int ")
                .Append(new System.String('*', type_.IndirectionLevel)) |> ignore

let emitDeclaration (t: T) (name: string): unit =
    let type_ = t._types.[name];
    if type_.IsFunction then
        t._sb.Append("void")
            .Append(" ")
            .Append("(")
            .Append(new System.String('*', type_.IndirectionLevel + 1))
            .Append(name)
            .Append(")")
            .Append("()") |> ignore
    else
        t._sb.Append("int ")
            .Append(new System.String('*', type_.IndirectionLevel))
            .Append(name) |> ignore

let emitStatement (t: T) (statement: IStatementNode): unit =
    match statement with
    | :? AssignmentNode as assignment -> emitAssignment t assignment
    | :? DoWhileNode as doWhile -> emitDoWhile t doWhile
    | :? FunctionCallNode as call -> emitFunctionCall t call
    | :? IfThenElseNode as ifThenElse -> emitIfThenElse t ifThenElse
    | :? ReturnNode as ret -> emitReturn t ret
    | :? ScopeNode as scope -> emitScope t scope false
    | :? WhileNode as whileLoop -> emitWhile t whileLoop
    | _ -> raise (new NotSupportedException())

let emitAssignment (t :T) (assignment: AssignmentNode): unit =
    t._sb.Append(t._indent) |> ignore
    // TODO: reword how variables are passed and treated in each step
    if not (assignment.Var.Name.StartsWith("arg")) && t._declaredVariables.Add(assignment.Var.Name) then
        emitDeclaration t assignment.Var.Name
    else
        t._sb.Append(assignment.Var.Name) |> ignore
    t._sb.Append(" = ") |> ignore
    emitExpression t assignment.Expression
    t._sb.Append(";")
        .Append(Environment.NewLine) |> ignore

let emitDoWhile (t: T) (whileLoop: DoWhileNode): unit =
    t._sb.Append(t._indent)
        .Append("do")
        .Append(Environment.NewLine) |> ignore
    emitScope t whileLoop.Body true
    t._sb.Append(" ")
        .Append("while")
        .Append(" ")
        .Append("(") |> ignore
    emitExpression t whileLoop.Condition
    t._sb.Append(")")
        .Append(";")
        .Append(Environment.NewLine) |> ignore

let emitFunctionCall (t: T) (call: FunctionCallNode): unit =
    t._sb.Append(t._indent) |> ignore
    emitExpression t call.Function
    t._sb.Append("()")
        .Append(";")
        .Append(Environment.NewLine) |> ignore

let emitIfThenElse (t: T) (ifThenElse: IfThenElseNode): unit =
    t._sb.Append(t._indent)
        .Append("if")
        .Append(" ")
        .Append("(") |> ignore
    emitExpression t ifThenElse.Condition
    t._sb.Append(")")
        .Append(Environment.NewLine) |> ignore
    emitScope t ifThenElse.TrueBranch false
    if ifThenElse.FalseBranch.ChildrenCount > 0 then
        t._sb.Append(t._indent)
            .Append("else")
            .Append(Environment.NewLine) |> ignore
        emitScope t ifThenElse.FalseBranch false

let emitReturn (t: T) (ret: ReturnNode): unit =
    t._sb.Append(t._indent)
        .Append("return") |> ignore
    let isSome, var = ret.Var.TryGet()
    if isSome then
        t._sb.Append(" ")
            .Append(var.Name) |> ignore
    t._sb.Append(";")
        .Append(Environment.NewLine) |> ignore

let emitScope (t: T) (scope: ScopeNode) (skipNewline: bool): unit =
    t._sb.Append(t._indent)
        .Append("{")
        .Append(Environment.NewLine) |> ignore
    t._indentLevel <- t._indentLevel + 1
    let hasValue, indent = t._indents.TryGetValue(t._indentLevel)
    if hasValue then
        t._indent <- indent
    else
        t._indent <- new System.String(' ', t._indentLevel * IndentSize)
        t._indents.[t._indentLevel] <- t._indent

    for node in scope do
        emitStatement t node

    t._indentLevel <- t._indentLevel - 1
    t._indent <- t._indents.[t._indentLevel]
    t._sb.Append(t._indent)
        .Append("}") |> ignore
    if not skipNewline then
        t._sb.Append(Environment.NewLine) |> ignore

let emitWhile (t: T) (whileLoop: WhileNode): unit =
    t._sb.Append(t._indent)
        .Append("while")
        .Append(" ")
        .Append("(") |> ignore
    emitExpression t whileLoop.Condition
    t._sb.Append(")")
        .Append(Environment.NewLine) |> ignore
    emitScope t whileLoop.Body false

let emitExpression (t: T) (expression: IExpressionNode): unit =
    match expression with
    | :? BinaryOperatorNode as binary -> emitBinary t binary
    | :? DereferenceNode as dereference -> emitDereference t dereference
    | :? UnaryOperatorNode as unary -> emitUnary t unary
    | :? ValueNode as value -> emitValue t value
    | :? VarNode as var -> emitVar t var
    | _ -> raise (new NotSupportedException())

let emitBinary (t: T) (binary: BinaryOperatorNode): unit =
    emitExpression t binary.Left
    t._sb.Append(" ") |> ignore
    let op =
        match binary.Operator with
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
    t._sb.Append(op)
        .Append(" ") |> ignore
    emitExpression t binary.Right

let emitDereference (t: T) (dereference: DereferenceNode): unit =
    t._sb.Append("*")
        .Append("(") |> ignore
    emitExpression t dereference.Pointer
    t._sb.Append(")") |> ignore

let emitUnary (t: T) (unary: UnaryOperatorNode): unit =
    let op =
        match unary.Operator with
        | Operator.Negate -> "-"
        | Operator.Not -> "~"
        | _ -> raise (new NotSupportedException())
    t._sb.Append(op) |> ignore
    emitExpression t unary.Operand

let emitValue (t: T) (value: ValueNode): unit =
    t._sb.Append(value.Value) |> ignore

let emitVar (t: T) (var: VarNode): unit =
    t._sb.Append(var.Name) |> ignore
