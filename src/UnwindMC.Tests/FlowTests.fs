[<NUnit.Framework.TestFixture>]
module FlowTests

open System
open NDis86
open NUnit.Framework
open FlowAnalyzer
open FlowHelper
open IL

[<Test>]
let testFlowWithFunctionPointers (): unit =
    let nop0 = Nop
    let asn0 = Assign <| binary (Register OperandType.ESI) (Stack 0)
    let cmp0 = Compare <| binary (Register OperandType.ESI) (Stack 4)
    let br0 = Branch <| branch GreaterOrEqual 10uL
    let asn1 = Assign <| binary (Register OperandType.EAX) (Pointer (OperandType.ESI, 0))
    let cmp1 = Compare <| binary (Register OperandType.EAX) (Value 0)
    let br1 = Branch <| branch Equal 8uL
    let call = Call <| unary (Register OperandType.EAX)
    let add = Add <| binary (Register OperandType.ESI) (Value 4)
    let br3 = Branch <| branch Unconditional 2uL
    let nop1 = Nop
    let ret = Return <| unary (Register OperandType.EAX)

    let il = [| nop0; asn0; cmp0; br0; asn1; cmp1; br1; call; add; br3; nop1; ret |]

    let blocks = FlowAnalyzer.buildFlowGraph il

    let expected =
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

    assertFlowEqual expected blocks
    
[<Test>]
let testFlowFindMax (): unit =
    let nop0 = Nop
    let asn0 = Assign <| binary (Register OperandType.ECX) (Stack 4)
    let asn1 = Assign <| binary (Register OperandType.EAX) (Value Int32.MinValue)
    let cmp0 = Compare <| binary (Register OperandType.ECX) (Value 0)
    let br0 = Branch <| branch Equal 15uL
    let asn2 = Assign <| binary (Register OperandType.EDX) (Stack 0)
    let asn3 = Assign <| binary (Register OperandType.EAX) (Value Int32.MinValue)
    let asn4 = Assign <| binary (Register OperandType.ESI) (Pointer (OperandType.EDX, 0))
    let cmp1 = Compare <| binary (Register OperandType.EAX) (Register OperandType.ESI)
    let br1 = Branch <| branch GreaterOrEqual 11uL
    let asn5 = Assign <| binary (Register OperandType.EAX) (Register OperandType.ESI)
    let add0 = Add <| binary (Register OperandType.EDX) (Value 4)
    let sub0 = Subtract <| binary (Register OperandType.ECX) (Value 1)
    let cmp2 = Compare <| binary (Register OperandType.ECX) (Value 0)
    let br2 = Branch <| branch NotEqual 7uL
    let nop1 = Nop
    let ret = Return <| unary (Register OperandType.EAX)

    let il = [| nop0; asn0; asn1; cmp0; br0; asn2; asn3; asn4; cmp1; br1; asn5; add0; sub0; cmp2; br2; nop1; ret |]

    let blocks = FlowAnalyzer.buildFlowGraph il

    let expected =
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

    assertFlowEqual expected blocks
