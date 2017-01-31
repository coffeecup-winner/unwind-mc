using NDis86;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using UnwindMC.Analysis.IL;

namespace UnwindMC.Tests.Helpers
{
    public static class ILHelper
    {
        public static void AssertILEqual(ILInstruction expected, ILInstruction actual)
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

        public static ILInstruction Add(ILOperand target, ILOperand source)
        {
            return new ILInstruction(ILInstructionType.Add, target, source);
        }

        public static ILInstruction Assign(ILOperand target, ILOperand source)
        {
            return new ILInstruction(ILInstructionType.Assign, target, source);
        }

        public static ILInstruction Call(ILOperand target)
        {
            return new ILInstruction(ILInstructionType.Call, target);
        }

        public static ILInstruction Compare(ILOperand target, ILOperand source)
        {
            return new ILInstruction(ILInstructionType.Compare, target, source);
        }

        public static ILInstruction Return()
        {
            return new ILInstruction(ILInstructionType.Return);
        }

        public static ILOperand Pointer(OperandType reg, int offset = 0)
        {
            return ILOperand.FromPointer(reg, offset);
        }

        public static ILOperand Register(OperandType reg)
        {
            return ILOperand.FromRegister(reg);
        }

        public static ILOperand Stack(int offset)
        {
            return ILOperand.FromStack(offset);
        }

        public static ILOperand Value(int value)
        {
            return ILOperand.FromValue(value);
        }
    }
}
