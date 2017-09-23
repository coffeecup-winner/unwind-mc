module Transformer

open Ast

type T = {
    transformExpression: T -> Expression -> Expression
    transformStatement: T -> Statement -> Statement
}

let private transformExpression (t: T) (expr: Expression): Expression =
    match expr with
    | Binary (op, left, right) -> Binary (op, t.transformExpression t left, t.transformExpression t right)
    | Dereference expr -> Dereference (t.transformExpression t expr)
    | Unary (op, operand) -> Unary (op, t.transformExpression t operand)
    | Value value -> Value value
    | VarRef var -> VarRef var

let private transformStatement (t: T) (statement: Statement): Statement =
    match statement with
    | Assignment (var, expr) -> Assignment (var, t.transformExpression t expr)
    | Break -> Break
    | Continue -> Continue
    | DoWhile (statements, condition) -> DoWhile (statements |> ROL.map (t.transformStatement t), t.transformExpression t condition)
    | For (condition, modifier, body) -> For (t.transformExpression t condition, modifier |> ROL.map (t.transformStatement t), body |> ROL.map (t.transformStatement t))
    | FunctionCall expr -> FunctionCall (t.transformExpression t expr)
    | IfThenElse (condition, trueBranch, falseBranch) ->
        IfThenElse (t.transformExpression t condition, trueBranch |> ROL.map (t.transformStatement t), falseBranch |> ROL.map (t.transformStatement t))
    | Return var -> Return var
    | While (condition, statements) -> While (t.transformExpression t condition, statements |> ROL.map (t.transformStatement t))

let transform t = t.transformStatement t

let def: T = {
    transformExpression = transformExpression
    transformStatement = transformStatement
}
