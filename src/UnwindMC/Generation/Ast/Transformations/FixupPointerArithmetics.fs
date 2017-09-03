module FixupPointerArithmetics

open System.Collections.Generic
open Ast
open Type

let transformer (types: IReadOnlyDictionary<string, DataType>): Transformer.T =
    let fixup (op: Operator) (Var name as var) (value: int): Expression =
        let type_ = types.[name]
        if type_.indirectionLevel > 0 || type_.isFunction then
            if value % type_.size <> 0 then
                failwith "Value size must be divisible by type size"
            let newValue = Value (value / type_.size)
            Binary (op, VarRef var, newValue)
        else
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
