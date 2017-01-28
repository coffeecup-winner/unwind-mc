using System;
using NDis86;
using NUnit.Framework;
using UnwindMC.Analysis.IL;
using System.Collections.Generic;

namespace UnwindMC.Tests
{
    [TestFixture]
    public class ILDecompilationTests
    {
        [Test]
        public void TestILWithFunctionPointers()
        {
            var analyzer = AnalysisHelper.Analyze(@"
                004afa88                   56 push esi
                004afa89             8b742408 mov esi, [esp+0x8]
                004afa8d             3b74240c cmp esi, [esp+0xc]
                004afa91                 730d jae 0x4afaa0
                004afa93                 8b06 mov eax, [esi]
                004afa95                 85c0 test eax, eax
                004afa97                 7402 jz 0x4afa9b
                004afa99                 ffd0 call eax
                004afa9b               83c604 add esi, 0x4
                004afa9e                 ebed jmp 0x4afa8d
                004afaa0                   5e pop esi
                004afaa1                   c3 ret");
            var il = ILDecompiler.Decompile(analyzer.Graph, 0x4afa88);

            var asn0 = Assign(Register(OperandType.ESI), Stack(0));
            var cmp0 = Compare(Register(OperandType.ESI), Stack(4));
            var asn1 = Assign(Register(OperandType.EAX), Pointer(OperandType.ESI));
            var cmp1 = Compare(Register(OperandType.EAX), Value(0));
            var call = Call(Register(OperandType.EAX));
            var add = Add(Register(OperandType.ESI), Value(4));
            var ret = Return();

            asn0.AddNext(cmp0);
            cmp0.AddLeft(ILBranchType.GreaterOrEqual, ret);
            cmp0.AddRight(ILBranchType.GreaterOrEqual, asn1);
            asn1.AddNext(cmp1);
            cmp1.AddLeft(ILBranchType.Equal, add);
            cmp1.AddRight(ILBranchType.Equal, call);
            call.AddNext(add);
            add.AddNext(cmp0);

            AssertILEqual(asn0, il);
        }

        private static void AssertILEqual(ILInstruction expected, ILInstruction actual)
        {
            var verified = new HashSet<ILInstruction>();
            var queue = new Queue<Tuple<ILInstruction, ILInstruction>>();
            queue.Enqueue(Tuple.Create(expected, actual));
            verified.Add(expected);
            while (queue.Count > 0)
            {
                var pair = queue.Dequeue();
                var expectedInstr = pair.Item1;
                var actualInstr = pair.Item2;
                Assert.That(actualInstr.Type, Is.EqualTo(expectedInstr.Type));
                Assert.That(actualInstr.Branch, Is.EqualTo(expectedInstr.Branch));
                Assert.That(actualInstr.Source, Is.EqualTo(expectedInstr.Source));
                Assert.That(actualInstr.Target, Is.EqualTo(expectedInstr.Target));
                Assert.That(pair.Item2.Children.Count, Is.EqualTo(pair.Item1.Children.Count));
                var kv1 = pair.Item1.Children.GetEnumerator();
                var kv2 = pair.Item2.Children.GetEnumerator();
                while (kv1.MoveNext() && kv2.MoveNext())
                {
                    Assert.That(kv2.Current.Key, Is.EqualTo(kv1.Current.Key));
                    if (verified.Add(kv1.Current.Value))
                    {
                        queue.Enqueue(Tuple.Create(kv1.Current.Value, kv2.Current.Value));
                    }
                };
            }
        }

        private static ILInstruction Add(ILOperand target, ILOperand source)
        {
            return new ILInstruction(ILInstructionType.Add, target, source);
        }

        private static ILInstruction Assign(ILOperand target, ILOperand source)
        {
            return new ILInstruction(ILInstructionType.Assign, target, source);
        }

        private static ILInstruction Call(ILOperand target)
        {
            return new ILInstruction(ILInstructionType.Call, target);
        }

        private static ILInstruction Compare(ILOperand target, ILOperand source)
        {
            return new ILInstruction(ILInstructionType.Compare, target, source);
        }

        private static ILInstruction Return()
        {
            return new ILInstruction(ILInstructionType.Return);
        }

        private static ILOperand Pointer(OperandType reg, int offset = 0)
        {
            return ILOperand.FromPointer(reg, offset);
        }

        private static ILOperand Register(OperandType reg)
        {
            return ILOperand.FromRegister(reg);
        }

        private static ILOperand Stack(int offset)
        {
            return ILOperand.FromStack(offset);
        }

        private static ILOperand Value(int value)
        {
            return ILOperand.FromValue(value);
        }
    }
}
