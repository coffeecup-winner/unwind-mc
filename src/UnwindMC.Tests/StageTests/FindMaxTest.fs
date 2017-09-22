[<NUnit.Framework.TestFixture>]
module rec FindMaxTest

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
        08048400: 56                 push esi
        08048401: 8b 4c 24 0c        mov ecx, [esp+0xc]
        08048405: b8 00 00 00 80     mov eax, 0x80000000
        0804840a: 85 c9              test ecx, ecx
        0804840c: 74 16              jz 0x8048424
        0804840e: 8b 54 24 08        mov edx, [esp+0x8]
        08048412: b8 00 00 00 80     mov eax, 0x80000000
        08048417: 8b 32              mov esi, [edx]
        08048419: 39 f0              cmp eax, esi
        0804841b: 0f 4c c6           cmovl eax, esi
        0804841e: 83 c2 04           add edx, 0x4
        08048421: 49                 dec ecx
        08048422: 75 f3              jnz 0x8048417
        08048424: 5e                 pop esi
        08048425: c3                 ret"""

    let code = Decompiler.decompileFunction graph 0x8048400uL
    let expected =
        """int sub_8048400(int *arg0, int arg1)
        {
          int var0;
          int var1;
          int *var2;
          int var3;
        
          var0 = arg1;
          var1 = -2147483648;
          if (var0 != 0)
          {
            var2 = arg0;
            var1 = -2147483648;
            do
            {
              var3 = *(var2);
              if (var1 < var3)
              {
                var1 = var3;
              }
              var2 = var2 + 1;
              var0 = var0 - 1;
            } while (var0 != 0);
          }
          return var1;
        }
        """.StripIndent()
    assertSourceCodeEquals expected code

[<Test>]
let testStages (): unit =
    let graph = AnalysisHelper.analyze """
        08048400: 56                 push esi
        08048401: 8b 4c 24 0c        mov ecx, [esp+0xc]
        08048405: b8 00 00 00 80     mov eax, 0x80000000
        0804840a: 85 c9              test ecx, ecx
        0804840c: 74 16              jz 0x8048424
        0804840e: 8b 54 24 08        mov edx, [esp+0x8]
        08048412: b8 00 00 00 80     mov eax, 0x80000000
        08048417: 8b 32              mov esi, [edx]
        08048419: 39 f0              cmp eax, esi
        0804841b: 0f 4c c6           cmovl eax, esi
        0804841e: 83 c2 04           add edx, 0x4
        08048421: 49                 dec ecx
        08048422: 75 f3              jnz 0x8048417
        08048424: 5e                 pop esi
        08048425: c3                 ret"""
    let il = ILDecompiler.decompile graph 0x8048400uL

    let nop0 = Nop
    let asn0 = Assign <| binary (Register OperandType.ECX) (Argument 4)
    let asn1 = Assign <| binary (Register OperandType.EAX) (Value Int32.MinValue)
    let cmp0 = Compare <| binary (Register OperandType.ECX) (Value 0)
    let br0 = Branch <| branch Equal 15uL
    let asn2 = Assign <| binary (Register OperandType.EDX) (Argument 0)
    let asn3 = Assign <| binary (Register OperandType.EAX) (Value Int32.MinValue)
    let asn4 = Assign <| binary (Register OperandType.ESI) (ILOperand.Pointer (OperandType.EDX, 0))
    let cmp1 = Compare <| binary (Register OperandType.EAX) (Register OperandType.ESI)
    let br1 = Branch <| branch GreaterOrEqual 11uL
    let asn5 = Assign <| binary (Register OperandType.EAX) (Register OperandType.ESI)
    let add0 = Add <| binary (Register OperandType.EDX) (Value 4)
    let sub0 = Subtract <| binary (Register OperandType.ECX) (Value 1)
    let cmp2 = Compare <| binary (Register OperandType.ECX) (Value 0)
    let br2 = Branch <| branch NotEqual 7uL
    let nop1 = Nop
    let ret = Return <| unary (Register OperandType.EAX)

    let expected = [| nop0; asn0; asn1; cmp0; br0; asn2; asn3; asn4; cmp1; br1; asn5; add0; sub0; cmp2; br2; nop1; ret |]

    Assert.That(il, Is.EqualTo(expected))
    let [| nop0; asn0; asn1; cmp0; br0; asn2; asn3; asn4; cmp1; br1; asn5; add0; sub0; cmp2; br2; nop1; ret |] = il |> Seq.toArray

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

    let (blocks, types) = TypeResolver.resolveTypes blocks
    let parameterTypes = types.parameterTypes
    let variableTypes = types.variableTypes
    Assert.That(parameterTypes.Count, Is.EqualTo(2))
    Assert.That(parameterTypes.[0], Is.EqualTo(Pointer Int32))
    Assert.That(parameterTypes.[1], Is.EqualTo(Int32))

    Assert.That(variableTypes.Count, Is.EqualTo(5))
    Assert.That(variableTypes.[0], Is.EqualTo(Int32))
    Assert.That(variableTypes.[1], Is.EqualTo(Int32))
    Assert.That(variableTypes.[2], Is.EqualTo(Pointer Int32))
    Assert.That(variableTypes.[3], Is.EqualTo(Int32))
    Assert.That(variableTypes.[4], Is.EqualTo(Int32))

    let expected =
        [|
            SequentialBlock {
                instructions =
                    [|
                        Nop
                        Assign <| { binary (Register OperandType.ECX) (Argument 4) with leftId = 0; rightId = -1 }
                        Assign <| { binary (Register OperandType.EAX) (Value Int32.MinValue) with leftId = 1; rightId = -1 }
                    |]
            }
            ConditionalBlock {
                condition =
                    invertCondition
                        [|
                            Compare <| { binary (Register OperandType.ECX) (Value 0) with leftId = 0; rightId = -1 }
                            Branch <| branch Equal 15uL
                        |]
                trueBranch =
                    [|
                        SequentialBlock {
                            instructions =
                                [|
                                    Assign <| { binary (Register OperandType.EDX) (Argument 0) with leftId = 2; rightId = -1 }
                                    Assign <| { binary (Register OperandType.EAX) (Value Int32.MinValue) with leftId = 1; rightId = -1 }
                                |]
                        }
                        DoWhileBlock {
                            condition =
                                [|
                                    Compare <| { binary (Register OperandType.ECX) (Value 0) with leftId = 0; rightId = -1 }
                                    Branch <| branch NotEqual 7uL
                                |]
                            body =
                                [|
                                    SequentialBlock {
                                        instructions =
                                            [|
                                                Assign <| { binary (Register OperandType.ESI) (ILOperand.Pointer (OperandType.EDX, 0)) with leftId = 4; rightId = 2 }
                                            |]
                                    }
                                    ConditionalBlock {
                                        condition =
                                            invertCondition
                                                [|
                                                    Compare <| { binary (Register OperandType.EAX) (Register OperandType.ESI) with leftId = 1; rightId = 4 }
                                                    Branch <| branch GreaterOrEqual 11uL
                                                |]
                                        trueBranch =
                                            [|
                                                SequentialBlock {
                                                    instructions =
                                                        [|
                                                            Assign <| { binary (Register OperandType.EAX) (Register OperandType.ESI) with leftId = 1; rightId = 4 }
                                                        |]
                                                }
                                            |]
                                        falseBranch = [||]
                                    }
                                    SequentialBlock {
                                        instructions =
                                            [|
                                                Add <| { binary (Register OperandType.EDX) (Value 4) with leftId = 2; rightId = -1 }
                                                Subtract <| { binary (Register OperandType.ECX) (Value 1) with leftId = 0; rightId = -1 }
                                            |]
                                    }
                                |]
                        }
                    |]
                falseBranch = [||]
            }
            SequentialBlock {
                instructions =
                    [|
                        Nop
                        Return <| { unary (Register OperandType.EAX) with operandId = 1 }
                    |]
            }
        |]

    assertFlowEqual expected blocks

    let func = AstBuilder.buildAst "findMax" blocks parameterTypes [||] variableTypes
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

    let code = CppEmitter.emit func
    let expected =
        """int findMax(int *arg0, int arg1)
        {
          int var0;
          int var1;
          int *var2;
          int var3;
        
          var0 = arg1;
          var1 = -2147483648;
          if (var0 != 0)
          {
            var2 = arg0;
            var1 = -2147483648;
            do
            {
              var3 = *(var2);
              if (var1 < var3)
              {
                var1 = var3;
              }
              var2 = var2 + 1;
              var0 = var0 - 1;
            } while (var0 != 0);
          }
          return var1;
        }
        """.StripIndent()
    assertSourceCodeEquals expected code

let assertSourceCodeEquals (expected: string) (actual: string): unit =
    Assert.That(actual.Replace("\r\n", "\n"), Is.EqualTo(expected.Replace("\r\n", "\n")))
