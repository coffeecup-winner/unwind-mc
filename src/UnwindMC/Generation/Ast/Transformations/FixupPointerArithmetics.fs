module FixupPointerArithmetics

open System.Collections.Generic
open Ast
open Type

let transformer (types: IReadOnlyDictionary<string, DataType>): Transformer.T =
    let fixup (op: Operator) (Var name as var) (value: int): Expression =
        let type_ = types.[name]
        match type_ with
        | Pointer _
        | Function ->
            if value % sizeOf type_ <> 0 then
                failwith "Value size must be divisible by type size"
            let newValue = Value (value / sizeOf type_)
            Binary (op, VarRef var, newValue)
        | _ ->
            Binary (op, VarRef var, Value value)

    {
        Transformer.def with
            transformExpression =
                fun t expr ->
                    match expr with
                    | Binary (Operator.Add, VarRef var, Value value) -> fixup Operator.Add var value
                    | Binary (Operator.Add, Value value, VarRef var) -> fixup Operator.Add var value
                    | Binary (Operator.Subtract, VarRef var, Value value) -> fixup Operator.Subtract var value
                    | Binary (Operator.Subtract, Value value, VarRef var) -> fixup Operator.Subtract var value
                    | _ -> Transformer.def.transformExpression t expr
    }
