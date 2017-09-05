[<NUnit.Framework.TestFixture>]
module AstTests

open System
open NDis86
open NUnit.Framework
open Ast
open FlowAnalyzer
open IL
open ILHelper
open Type

[<Test>]
let testAstWithFunctionPointers (): unit =
    let asn0 = Assign (Register OperandType.ESI) (Stack 0)
    let cmp0 = Compare (Register OperandType.ESI) (Stack 4)
    let asn1 = Assign (Register OperandType.EAX) (Pointer (OperandType.ESI, 0))
    let cmp1 = Compare (Register OperandType.EAX) (Value 0)
    let call = Call (Register OperandType.EAX)
    let add = Add (Register OperandType.ESI) (Value 4)
    let ret = Return ()

    SetOrder [| asn0; cmp0; asn1; cmp1; call; add; ret |]

    asn0.targetId <- 0
    asn0.sourceId <- -1
    cmp0.targetId <- 0
    cmp0.sourceId <- -1
    asn1.targetId <- 1
    asn1.sourceId <- 0
    cmp1.targetId <- 1
    cmp1.sourceId <- -1
    call.targetId <- 1
    call.sourceId <- -1
    add.targetId <- 0
    add.sourceId <- -1
    ret.targetId <- -1
    ret.sourceId <- -1

    asn0.defaultChild <- Some cmp0
    cmp0.defaultChild <- Some ret
    cmp0.condition <- ILBranchType.Less
    cmp0.conditionalChild <- Some asn1
    asn1.defaultChild <- Some cmp1
    cmp1.defaultChild <- Some add
    cmp1.condition <- ILBranchType.NotEqual
    cmp1.conditionalChild <- Some call
    call.defaultChild <- Some add
    add.defaultChild <- Some cmp0

    let blocks =
        [|
            SequentialBlock { instructions = [| asn0 |] }
            WhileBlock {
                condition = cmp0
                children =
                    [|
                        SequentialBlock { instructions = [| asn1 |] }
                        ConditionalBlock {
                            condition = cmp1
                            trueBranch = [| SequentialBlock { instructions = [| call |] } |]
                            falseBranch = [| SequentialBlock { instructions = [||] } |]
                        }
                        SequentialBlock { instructions = [| add |] }
                    |]
            }
            SequentialBlock { instructions = [| ret |] }
        |]

    let parameterTypes =
        [|
            { isFunction = true; indirectionLevel = 1; size = 4 }
            { isFunction = true; indirectionLevel = 1; size = 4 }
        |]

    let variableTypes =
        [|
            { isFunction = true; indirectionLevel = 1; size = 4 }
            { isFunction = true; indirectionLevel = 0; size = 4 }
        |]

    let func = AstBuilder.buildAst "" blocks parameterTypes [||] variableTypes
    let expected =
        [|
            Assignment (Var "var0", VarRef (Var "arg0"))
            While
                (
                    Binary (Operator.Less, VarRef (Var "var0"), VarRef (Var "arg1")),
                    [|
                        Assignment (Var "var1", Dereference (VarRef (Var "var0")))
                        IfThenElse
                            (
                                Binary (Operator.NotEqual, VarRef (Var "var1"), Expression.Value 0),
                                [| FunctionCall (VarRef (Var "var1")) |],
                                [||]
                            )
                        Assignment (Var "var0", Binary (Operator.Add, VarRef (Var "var0"), Expression.Value 1))
                    |]
                )
            Statement.Return None
        |]
    AstHelper.assertAstEqual expected func.body

[<Test>]
let testAstFindMax (): unit =
    let asn0 = Assign (Register OperandType.ECX) (Stack 4)
    let asn1 = Assign (Register OperandType.EAX) (Value Int32.MinValue)
    let cmp0 = Compare (Register OperandType.ECX) (Value 0)
    let asn2 = Assign (Register OperandType.EDX) (Stack 0)
    let asn3 = Assign (Register OperandType.EAX) (Value Int32.MinValue)
    let asn4 = Assign (Register OperandType.ESI) (Pointer (OperandType.EDX, 0))
    let cmp1 = Compare (Register OperandType.EAX) (Register OperandType.ESI)
    let asn5 = Assign (Register OperandType.EAX) (Register OperandType.ESI)
    let add0 = Add (Register OperandType.EDX) (Value 4)
    let sub0 = Subtract (Register OperandType.ECX) (Value 1)
    let cmp2 = Compare (Register OperandType.ECX) (Value 0)
    let ret = Return ()

    asn0.targetId <- 1
    asn0.sourceId <- -1
    asn1.targetId <- 0
    asn1.sourceId <- -1
    cmp0.targetId <- 1
    cmp0.sourceId <- -1
    asn2.targetId <- 2
    asn2.sourceId <- -1
    asn3.targetId <- 0
    asn3.sourceId <- -1
    asn4.targetId <- 3
    asn4.sourceId <- 2
    cmp1.targetId <- 0
    cmp1.sourceId <- 3
    asn5.targetId <- 0
    asn5.sourceId <- 3
    add0.targetId <- 2
    add0.sourceId <- -1
    sub0.targetId <- 1
    sub0.sourceId <- -1
    cmp2.targetId <- 1
    cmp2.sourceId <- -1
    ret.targetId <- -1
    ret.sourceId <- 0

    SetOrder [| asn0; asn1; cmp0; asn2; asn3; asn4; cmp1; asn5; add0; sub0; cmp2; ret |]

    asn0.defaultChild <- Some asn1
    asn1.defaultChild <- Some cmp0
    cmp0.defaultChild <- Some ret
    cmp0.condition <- ILBranchType.NotEqual
    cmp0.conditionalChild <- Some asn2
    asn2.defaultChild <- Some asn3
    asn3.defaultChild <- Some asn4
    asn4.defaultChild <- Some cmp1
    cmp1.defaultChild <- Some add0
    cmp1.condition <- ILBranchType.Less
    cmp1.conditionalChild <- Some asn5
    asn5.defaultChild <- Some add0
    add0.defaultChild <- Some sub0
    sub0.defaultChild <- Some cmp2
    cmp2.defaultChild <- Some ret
    cmp2.condition <- ILBranchType.NotEqual
    cmp2.conditionalChild <- Some asn4

    let blocks =
        [|
            SequentialBlock { instructions = [| asn0; asn1 |] }
            ConditionalBlock {
                condition = cmp0
                trueBranch =
                    [|
                        SequentialBlock { instructions = [| asn2; asn3 |] }
                        DoWhileBlock {
                            condition = cmp2
                            children =
                                [|
                                    SequentialBlock { instructions = [| asn4 |] }
                                    ConditionalBlock {
                                        condition = cmp1
                                        trueBranch = [| SequentialBlock { instructions = [| asn5 |] } |]
                                        falseBranch = [| SequentialBlock { instructions = [||] } |]
                                    }
                                    SequentialBlock { instructions = [| add0; sub0 |] }
                                |]
                        }
                    |]
                falseBranch = [| SequentialBlock { instructions = [||] } |]
            }
            SequentialBlock { instructions = [| ret |] }
        |]

    let parameterTypes =
        [|
            { isFunction = false; indirectionLevel = 1; size = 4 }
            { isFunction = false; indirectionLevel = 0; size = 4 }
        |]

    let variableTypes =
        [|
            { isFunction = false; indirectionLevel = 0; size = 4 }
            { isFunction = false; indirectionLevel = 0; size = 4 }
            { isFunction = false; indirectionLevel = 1; size = 4 }
            { isFunction = false; indirectionLevel = 0; size = 4 }
        |]

    let func = AstBuilder.buildAst "" blocks parameterTypes [||] variableTypes
    let expected =
        [|
            Assignment (Var "var0", VarRef (Var "arg1"))
            Assignment (Var "var1", Expression.Value Int32.MinValue)
            IfThenElse
                (
                    Binary (Operator.NotEqual, VarRef (Var "var0"), Expression.Value 0),
                    [|
                        Assignment (Var "var2", VarRef (Var "arg0"))
                        Assignment (Var "var1", Expression.Value Int32.MinValue)
                        DoWhile
                            (
                                [|
                                    Assignment (Var "var3", Dereference (VarRef (Var "var2")))
                                    IfThenElse
                                        (
                                            Binary (Operator.Less, VarRef (Var "var1"), VarRef (Var "var3")),
                                            [| Assignment (Var "var1", VarRef (Var "var3")) |],
                                            [||]
                                        )
                                    Assignment (Var "var2", Binary (Operator.Add, VarRef (Var "var2"), Expression.Value 1))
                                    Assignment (Var "var0", Binary (Operator.Subtract, VarRef (Var "var0"), Expression.Value 1))
                                |],
                                Binary (Operator.NotEqual, VarRef (Var "var0"), Expression.Value 0)
                            )
                    |],
                    [||]
                )
            Statement.Return (Some (Var "var1"))
        |]
    AstHelper.assertAstEqual expected func.body
