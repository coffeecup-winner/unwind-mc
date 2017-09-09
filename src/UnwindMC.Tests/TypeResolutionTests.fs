[<NUnit.Framework.TestFixture>]
module TypeResolutionTests

open System
open NDis86
open NUnit.Framework
open FlowAnalyzer
open IL

[<Test>]
let testResolutionWithFunctionPointers (): unit =
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

    let blocks =
        [|
            SequentialBlock { instructions = [| nop0; asn0 |] }
            WhileBlock {
                condition = invertCondition [| cmp0; br0 |]
                children =
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

    let types = TypeResolver.resolveTypes blocks
    let parameterTypes = types.parameterTypes
    let variableTypes = types.variableTypes
    Assert.That(parameterTypes.Count, Is.EqualTo(2))
    Assert.That(parameterTypes.[0].isFunction, Is.True)
    Assert.That(parameterTypes.[0].indirectionLevel, Is.EqualTo(1))
    Assert.That(parameterTypes.[1].isFunction, Is.True)
    Assert.That(parameterTypes.[1].indirectionLevel, Is.EqualTo(1))

    Assert.That(variableTypes.Count, Is.EqualTo(2))
    Assert.That(variableTypes.[0].isFunction, Is.True)
    Assert.That(variableTypes.[0].indirectionLevel, Is.EqualTo(1))
    Assert.That(variableTypes.[1].isFunction, Is.True)
    Assert.That(variableTypes.[1].indirectionLevel, Is.EqualTo(0))

    match asn0 with
    | Assign { leftId = 0; rightId = -1 } -> ()
    | _ -> Assert.Fail()
    match cmp0 with
    | Compare { leftId = 0; rightId = -1 } -> ()
    | _ -> Assert.Fail()
    match asn1 with
    | Assign { leftId = 1; rightId = 0 } -> ()
    | _ -> Assert.Fail()
    match cmp1 with
    | Compare { leftId = 1; rightId = -1 } -> ()
    | _ -> Assert.Fail()
    match call with
    | Call { operandId = 1 } -> ()
    | _ -> Assert.Fail()
    match add with
    | Add { leftId = 0; rightId = -1 } -> ()
    | _ -> Assert.Fail()
    match ret with
    | Return { operandId = -1 } -> ()
    | _ -> Assert.Fail()

[<Test>]
let testResolutionFindMax (): unit =
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
                            children =
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

    let types = TypeResolver.resolveTypes blocks
    let parameterTypes = types.parameterTypes
    let variableTypes = types.variableTypes
    Assert.That(parameterTypes.Count, Is.EqualTo(2))
    Assert.That(parameterTypes.[0].isFunction, Is.False)
    Assert.That(parameterTypes.[0].indirectionLevel, Is.EqualTo(1))
    Assert.That(parameterTypes.[1].isFunction, Is.False)
    Assert.That(parameterTypes.[1].indirectionLevel, Is.EqualTo(0))

    Assert.That(variableTypes.Count, Is.EqualTo(4))
    Assert.That(variableTypes.[0].isFunction, Is.False)
    Assert.That(variableTypes.[0].indirectionLevel, Is.EqualTo(0))
    Assert.That(variableTypes.[1].isFunction, Is.False)
    Assert.That(variableTypes.[1].indirectionLevel, Is.EqualTo(0))
    Assert.That(variableTypes.[2].isFunction, Is.False)
    Assert.That(variableTypes.[2].indirectionLevel, Is.EqualTo(1))
    Assert.That(variableTypes.[3].isFunction, Is.False)
    Assert.That(variableTypes.[3].indirectionLevel, Is.EqualTo(0))

    match asn0 with
    | Assign { leftId = 1; rightId = -1 } -> ()
    | _ -> Assert.Fail()
    match asn1 with
    | Assign { leftId = 0; rightId = -1 } -> ()
    | _ -> Assert.Fail()
    match cmp0 with
    | Compare { leftId = 1; rightId = -1 } -> ()
    | _ -> Assert.Fail()
    match asn2 with
    | Assign { leftId = 2; rightId = -1 } -> ()
    | _ -> Assert.Fail()
    match asn3 with
    | Assign { leftId = 0; rightId = -1 } -> ()
    | _ -> Assert.Fail()
    match asn4 with
    | Assign { leftId = 3; rightId = 2 } -> ()
    | _ -> Assert.Fail()
    match cmp1 with
    | Compare { leftId = 0; rightId = 3 } -> ()
    | _ -> Assert.Fail()
    match asn5 with
    | Assign { leftId = 0; rightId = 3 } -> ()
    | _ -> Assert.Fail()
    match add0 with
    | Add { leftId = 2; rightId = -1 } -> ()
    | _ -> Assert.Fail()
    match sub0 with
    | Subtract { leftId = 1; rightId = -1 } -> ()
    | _ -> Assert.Fail()
    match cmp2 with
    | Compare { leftId = 1; rightId = -1 } -> ()
    | _ -> Assert.Fail()
    match ret with
    | Return { operandId = 0 } -> ()
    | _ -> Assert.Fail()
