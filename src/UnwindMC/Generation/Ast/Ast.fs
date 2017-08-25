module Ast

open System.Collections.Generic

type Operator
    = Equal
    | NotEqual
    | Less
    | LessOrEqual
    | Greater
    | GreaterOrEqual

    | Not
    | And
    | Or
    | Xor
    | ShiftLeft
    | ShiftRight

    | Negate
    | Add
    | Subtract
    | Multiply
    | Divide
    | Modulo

type Var = Var of string

type Expression
    = Binary of Operator * Expression * Expression
    | Dereference of Expression
    | Unary of Operator * Expression
    | Value of int
    | VarRef of Var

type Statement
    = Assignment of Var * Expression
    | DoWhile of Statement * Expression
    | FunctionCall of Expression
    | IfThenElse of Expression * Statement * Statement
    | Return of Var option
    | Scope of IReadOnlyList<Statement>
    | While of Expression * Statement
