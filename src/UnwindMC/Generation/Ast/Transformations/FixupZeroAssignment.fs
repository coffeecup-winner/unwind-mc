module FixupZeroAssignment

open Ast

let transformer: Transformer.T =
    {
        Transformer.def with
            transformStatement =
                fun t stmt ->
                    match stmt with
                    | Assignment (Var name, expr) ->
                        match expr with
                        | Binary (Operator.And, VarRef (Var v), Value 0) when name = v -> Assignment (Var v, Value 0)
                        | _ -> Transformer.def.transformStatement t stmt
                    | _ -> Transformer.def.transformStatement t stmt
    }
