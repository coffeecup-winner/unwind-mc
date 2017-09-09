[<NUnit.Framework.TestFixture>]
module AstTests

open System
open NDis86
open NUnit.Framework
open Ast
open FlowAnalyzer
open IL
open Type

[<Test>]
let testAstWithFunctionPointers (): unit =
    let nop0 = Nop
    let asn0 = Assign { left = Register OperandType.ESI; right = Stack 0; leftId = 0; rightId = -1 }
    let cmp0 = Compare { left = Register OperandType.ESI; right = Stack 4; leftId = 0; rightId = -1 }
    let br0 = Branch <| branch GreaterOrEqual 10uL
    let asn1 = Assign { left = Register OperandType.EAX; right = Pointer (OperandType.ESI, 0); leftId = 1; rightId = 0 }
    let cmp1 = Compare { left = Register OperandType.EAX; right = Value 0; leftId = 1; rightId = -1 }
    let br1 = Branch <| branch Equal 8uL
    let call = Call { operand = Register OperandType.EAX; operandId = 1 }
    let add = Add { left = Register OperandType.ESI; right = Value 4; leftId = 0; rightId = -1 }
    let br3 = Branch <| branch Unconditional 2uL
    let nop1 = Nop
    let ret = Return { operand = Register OperandType.EAX; operandId = -1 }

    let blocks =
        [|
            SequentialBlock { instructions = [| nop0; asn0 |] }
            WhileBlock {
                condition = invertCondition [| cmp0; br0 |]
                body =
                    [|
                        SequentialBlock { instructions = [| asn1 |] }
                        ConditionalBlock {
                            condition = invertCondition [| cmp1; br1 |]
                            trueBranch = [| SequentialBlock { instructions = [| call |] } |]
                            falseBranch = [||]
                        }
                        SequentialBlock { instructions = [| add |] }
                    |]
            }
            SequentialBlock { instructions = [| nop1; ret |] }
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
    let nop0 = Nop
    let asn0 = Assign { left = Register OperandType.ECX; right = Stack 4; leftId = 1; rightId = -1 }
    let asn1 = Assign { left = Register OperandType.EAX; right = Value Int32.MinValue; leftId = 0; rightId = -1 }
    let cmp0 = Compare { left = Register OperandType.ECX; right = Value 0; leftId = 1; rightId = -1 }
    let br0 = Branch <| branch Equal 15uL
    let asn2 = Assign { left = Register OperandType.EDX; right = Stack 0; leftId = 2; rightId = -1 }
    let asn3 = Assign { left = Register OperandType.EAX; right = Value Int32.MinValue; leftId = 0; rightId = -1 }
    let asn4 = Assign { left = Register OperandType.ESI; right = Pointer (OperandType.EDX, 0); leftId = 3; rightId = 2 }
    let cmp1 = Compare { left = Register OperandType.EAX; right = Register OperandType.ESI; leftId = 0; rightId = 3 }
    let br1 = Branch <| branch GreaterOrEqual 11uL
    let asn5 = Assign { left = Register OperandType.EAX; right = Register OperandType.ESI; leftId = 0; rightId = 3 }
    let add0 = Add { left = Register OperandType.EDX; right = Value 4; leftId = 2; rightId = -1 }
    let sub0 = Subtract { left = Register OperandType.ECX; right = Value 1; leftId = 1; rightId = -1 }
    let cmp2 = Compare { left = Register OperandType.ECX; right = Value 0; leftId = 1; rightId = -1 }
    let br2 = Branch <| branch NotEqual 7uL
    let nop1 = Nop
    let ret = Return { operand = Register OperandType.EAX; operandId = 0 }

    let blocks =
        [|
            SequentialBlock { instructions = [| nop0; asn0; asn1 |] }
            ConditionalBlock {
                condition = invertCondition [| cmp0; br0 |]
                trueBranch =
                    [|
                        SequentialBlock { instructions = [| asn2; asn3 |] }
                        DoWhileBlock {
                            condition = [| cmp2; br2 |]
                            body =
                                [|
                                    SequentialBlock { instructions = [| asn4 |] }
                                    ConditionalBlock {
                                        condition = invertCondition [| cmp1; br1 |]
                                        trueBranch = [| SequentialBlock { instructions = [| asn5 |] } |]
                                        falseBranch = [||]
                                    }
                                    SequentialBlock { instructions = [| add0; sub0 |] }
                                |]
                        }
                    |]
                falseBranch = [||]
            }
            SequentialBlock { instructions = [| nop1; ret |] }
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
