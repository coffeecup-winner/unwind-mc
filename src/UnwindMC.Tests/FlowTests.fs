[<NUnit.Framework.TestFixture>]
module FlowTests

open System
open NDis86
open NUnit.Framework
open FlowAnalyzer
open FlowHelper
open IL
open ILHelper

[<Test>]
let testFlowWithFunctionPointers (): unit =
    let asn0 = Assign (Register OperandType.ESI) (Stack 0)
    let cmp0 = Compare (Register OperandType.ESI) (Stack 4)
    let asn1 = Assign (Register OperandType.EAX) (Pointer (OperandType.ESI, 0))
    let cmp1 = Compare (Register OperandType.EAX) (Value 0)
    let call = Call (Register OperandType.EAX)
    let add = Add (Register OperandType.ESI) (Value 4)
    let ret = Return ()

    SetOrder [| asn0; cmp0; asn1; cmp1; call; add; ret |]

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

    let blocks = FlowAnalyzer.buildFlowGraph asn0

    let expected =
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

    assertFlowEqual expected blocks
    
[<Test>]
let testFlowFindMax (): unit =
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

    let blocks = FlowAnalyzer.buildFlowGraph asn0

    let expected =
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

    assertFlowEqual expected blocks
