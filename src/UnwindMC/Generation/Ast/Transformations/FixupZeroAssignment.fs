module FixupZeroAssignment

open Ast

let transform: Statement -> Statement =
    Transformer.transform {
        Transformer.def with transformAssignment =
            fun t (Assignment (Var name, expr) as assignment) ->
                match expr with
                | Binary (Operator.And, VarRef (Var v), Value 0) when name = v -> Assignment (Var v, Value (0))
                | e -> Transformer.def.transformAssignment t assignment
    }
