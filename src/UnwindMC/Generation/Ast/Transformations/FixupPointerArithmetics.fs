﻿module FixupPointerArithmetics

open System
open System.Collections.Generic
open UnwindMC.Analysis
open Ast

let transform (types: IReadOnlyDictionary<string, Data.Type>): Statement -> Statement =
    let fixup (op: Operator) (Var name as var) (value: int): Expression =
        let type_ = types.[name];
        if type_.IndirectionLevel > 0 || type_.IsFunction then
            if value % type_.Size <> 0 then
                raise (new InvalidOperationException("Value size must be divisible by type size"))
            let newValue = Value (value / type_.Size)
            Binary (op, VarRef var, newValue);
        else
            Binary (op, VarRef var, Value value)

    Transformer.transform {
        Transformer.def with transformBinary =
            fun t (Binary (op, left, right) as binary) ->
                match binary with
                | Binary (Operator.Add, VarRef var, Value value) -> fixup op var value
                | Binary (Operator.Add, Value value, VarRef var) -> fixup op var value
                | Binary (Operator.Subtract, VarRef var, Value value) -> fixup op var value
                | Binary (Operator.Subtract, Value value, VarRef var) -> fixup op var value
                | _ -> Transformer.def.transformBinary t binary
    }
