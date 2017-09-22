[<NUnit.Framework.TestFixture>]
module rec FunctionArrayTest

#nowarn "25"

open System
open NDis86
open NUnit.Framework
open Ast
open FlowAnalyzer
open FlowHelper
open IL
open Type

[<Test>]
let testEndToEnd (): unit =
    let graph = AnalysisHelper.analyze """
        00400000: 56                 push esi
        00400001: 8b 74 24 08        mov esi, [esp+0x8]
        00400005: 3b 74 24 0c        cmp esi, [esp+0xc]
        00400009: 73 0d              jae 0x400018
        0040000b: 8b 06              mov eax, [esi]
        0040000d: 85 c0              test eax, eax
        0040000f: 74 02              jz 0x400013
        00400011: ff d0              call eax
        00400013: 83 c6 04           add esi, 0x4
        00400016: eb ed              jmp 0x400005
        00400018: 5e                 pop esi
        00400019: c3                 ret"""

    let code = Decompiler.decompileFunction graph 0x400000uL
    let expected =
        """void sub_400000(void (**arg0)(), void (**arg1)())
        {
          void (**var0)();
          void (*var1)();
        
          var0 = arg0;
          while (var0 < arg1)
          {
            var1 = *(var0);
            if (var1 != 0)
            {
              var1();
            }
            var0 = var0 + 1;
          }
          return;
        }
        """.StripIndent()
    assertSourceCodeEquals expected code

[<Test>]
let testStages (): unit =
    let graph = AnalysisHelper.analyze """
        00400000: 56                 push esi
        00400001: 8b 74 24 08        mov esi, [esp+0x8]
        00400005: 3b 74 24 0c        cmp esi, [esp+0xc]
        00400009: 73 0d              jae 0x400018
        0040000b: 8b 06              mov eax, [esi]
        0040000d: 85 c0              test eax, eax
        0040000f: 74 02              jz 0x400013
        00400011: ff d0              call eax
        00400013: 83 c6 04           add esi, 0x4
        00400016: eb ed              jmp 0x400005
        00400018: 5e                 pop esi
        00400019: c3                 ret"""
    let il = ILDecompiler.decompile graph 0x400000uL

    let nop0 = Nop
    let asn0 = Assign <| binary (Register OperandType.ESI) (Argument 0)
    let cmp0 = Compare <| binary (Register OperandType.ESI) (Argument 4)
    let br0 = Branch <| branch GreaterOrEqual 10uL
    let asn1 = Assign <| binary (Register OperandType.EAX) (ILOperand.Pointer (OperandType.ESI, 0))
    let cmp1 = Compare <| binary (Register OperandType.EAX) (Value 0)
    let br1 = Branch <| branch Equal 8uL
    let call = Call <| unary (Register OperandType.EAX)
    let add = Add <| binary (Register OperandType.ESI) (Value 4)
    let br3 = Branch <| branch Unconditional 2uL
    let nop1 = Nop
    let ret = Return <| unary (Register OperandType.EAX)

    let expected = [| nop0; asn0; cmp0; br0; asn1; cmp1; br1; call; add; br3; nop1; ret |]

    Assert.That(il, Is.EqualTo(expected))
    let [| nop0; asn0; cmp0; br0; asn1; cmp1; br1; call; add; br3; nop1; ret |] = il |> Seq.toArray

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

    let (blocks, types) = TypeResolver.resolveTypes blocks
    let parameterTypes = types.parameterTypes
    let variableTypes = types.variableTypes
    Assert.That(parameterTypes.Count, Is.EqualTo(2))
    Assert.That(parameterTypes.[0], Is.EqualTo(Pointer Function))
    Assert.That(parameterTypes.[1], Is.EqualTo(Pointer Function))

    Assert.That(variableTypes.Count, Is.EqualTo(2))
    Assert.That(variableTypes.[0], Is.EqualTo(Pointer Function))
    Assert.That(variableTypes.[1], Is.EqualTo(Function))

    let expected =
        [|
            SequentialBlock {
                instructions =
                    [|
                        Nop
                        Assign <| { binary (Register OperandType.ESI) (Argument 0) with leftId = 0; rightId = -1 }
                    |]
            }
            WhileBlock {
                condition =
                    invertCondition
                        [|
                            Compare <| { binary (Register OperandType.ESI) (Argument 4) with leftId = 0; rightId = -1 }
                            Branch <| branch GreaterOrEqual 10uL
                        |]
                body =
                    [|
                        SequentialBlock {
                            instructions =
                                [|
                                    Assign <| { binary (Register OperandType.EAX) (ILOperand.Pointer (OperandType.ESI, 0)) with leftId = 1; rightId = 0 }
                                |]
                        }
                        ConditionalBlock {
                            condition =
                                invertCondition
                                    [|
                                        Compare <| { binary (Register OperandType.EAX) (Value 0) with leftId = 1; rightId = -1 }
                                        Branch <| branch Equal 8uL
                                    |]
                            trueBranch =
                                [|
                                    SequentialBlock {
                                        instructions =
                                            [|
                                                Call <| { unary (Register OperandType.EAX) with operandId = 1 }
                                            |]
                                    }
                                |]
                            falseBranch = [||]
                        }
                        SequentialBlock {
                            instructions =
                                [|
                                    Add <| { binary (Register OperandType.ESI) (Value 4) with leftId = 0; rightId = -1 }
                                |]
                        }
                    |]
            }
            SequentialBlock {
                instructions =
                    [|
                        Nop
                        Return <| { unary (Register OperandType.EAX) with operandId = -1 }
                    |]
            }
        |]

    assertFlowEqual expected blocks

    let func = AstBuilder.buildAst "functionArray" blocks parameterTypes [||] variableTypes
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

    let code = CppEmitter.emit func
    let expected =
        """void functionArray(void (**arg0)(), void (**arg1)())
        {
          void (**var0)();
          void (*var1)();
        
          var0 = arg0;
          while (var0 < arg1)
          {
            var1 = *(var0);
            if (var1 != 0)
            {
              var1();
            }
            var0 = var0 + 1;
          }
          return;
        }
        """.StripIndent()
    assertSourceCodeEquals expected code

let assertSourceCodeEquals (expected: string) (actual: string): unit =
    Assert.That(actual.Replace("\r\n", "\n"), Is.EqualTo(expected.Replace("\r\n", "\n")))
