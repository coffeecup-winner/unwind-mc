module Transformer

open Ast

type T = {
    transformBinary: T -> Expression -> Expression
    transformDereference: T -> Expression -> Expression
    transformUnary: T -> Expression -> Expression
    transformValue: T -> Expression -> Expression
    transformVarRef: T -> Expression -> Expression

    transformAssignment: T -> Statement -> Statement
    transformDoWhile: T -> Statement -> Statement
    transformFunctionCall: T -> Statement -> Statement
    transformIfThenElse: T -> Statement -> Statement
    transformReturn: T -> Statement -> Statement
    transformScope: T -> Statement -> Statement
    transformWhile: T -> Statement -> Statement
}

let private transformExpression (t: T) (expr: Expression): Expression =
    match expr with
    | Binary _ as binary -> t.transformBinary t binary
    | Dereference _ as deref -> t.transformDereference t deref
    | Unary _ as unary -> t.transformUnary t unary
    | Value _ as value -> t.transformValue t value
    | VarRef _ as var -> t.transformVarRef t var

let private transformStatement (t: T) (statement: Statement): Statement =
    match statement with
    | Assignment _ as assignment -> t.transformAssignment t assignment
    | DoWhile _ as doWhile -> t.transformDoWhile t doWhile
    | FunctionCall _ as call -> t.transformFunctionCall t call
    | IfThenElse _ as ifThenElse -> t.transformIfThenElse t ifThenElse
    | Return _ as ret -> t.transformReturn t ret
    | Scope _ as scope -> t.transformScope t scope
    | While _ as whileLoop -> t.transformWhile t whileLoop

let transform = transformStatement

let def: T = {
    transformBinary = fun t (Binary (op, left, right)) -> Binary (op, transformExpression t left, transformExpression t right)
    transformDereference = fun t (Dereference expr) -> Dereference (transformExpression t expr)
    transformUnary = fun t (Unary (op, operand)) -> Unary (op, transformExpression t operand)
    transformValue = fun _ v -> v
    transformVarRef = fun _ v -> v

    transformAssignment = fun t (Assignment (var, expr)) -> Assignment (var, transformExpression t expr)
    transformDoWhile = fun t (DoWhile (statement, condition)) -> DoWhile (transformStatement t statement, transformExpression t condition)
    transformFunctionCall = fun t (FunctionCall expr) -> FunctionCall (transformExpression t expr)
    transformIfThenElse = fun t (IfThenElse (condition, trueBranch, falseBranch)) ->
        IfThenElse (transformExpression t condition, transformStatement t trueBranch, transformStatement t falseBranch)
    transformReturn = fun _ r -> r
    transformScope = fun t (Scope statements) -> Scope (statements |> Seq.map (transformStatement t) |> Seq.toArray)
    transformWhile = fun t (While (condition, statement)) -> While (transformExpression t condition, transformStatement t statement)
}
