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
                Assert.That(actualInstr.Condition, Is.EqualTo(expectedInstr.Condition));
                Assert.That(actualInstr.DefaultChild == null, Is.EqualTo(expectedInstr.DefaultChild == null));
                Assert.That(actualInstr.ConditionalChild == null, Is.EqualTo(expectedInstr.ConditionalChild == null));
                Assert.That(actualInstr.Order, Is.EqualTo(expectedInstr.Order));
                if (expectedInstr.DefaultChild != null && verified.Add(expectedInstr.DefaultChild))
                {
                    queue.Enqueue(Tuple.Create(expectedInstr.DefaultChild, actualInstr.DefaultChild));
                }
                if (expectedInstr.ConditionalChild != null && verified.Add(expectedInstr.ConditionalChild))
                {
                    queue.Enqueue(Tuple.Create(expectedInstr.ConditionalChild, actualInstr.ConditionalChild));
                }
            }
        }

        public static void AssertVarIds(ILInstruction asn0, int targetId, int sourceId)
        {
            Assert.That(asn0.TargetId, Is.EqualTo(targetId));
            Assert.That(asn0.SourceId, Is.EqualTo(sourceId));
        }

        public static void SetOrder(params ILInstruction[] instructions)
        {
            for (int i = 0; i < instructions.Length; i++)
            {
                instructions[i].SetOrder(i);
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

        public static ILInstruction Subtract(ILOperand target, ILOperand source)
        {
            return new ILInstruction(ILInstructionType.Subtract, target, source);
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
