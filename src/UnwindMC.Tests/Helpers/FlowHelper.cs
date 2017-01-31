using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;
using UnwindMC.Analysis.Flow;
using UnwindMC.Analysis.IL;

namespace UnwindMC.Tests.Helpers
{
    public static class FlowHelper
    {
        public static void AssertFlowEqual(IReadOnlyList<IBlock> expected, IReadOnlyList<IBlock> blocks)
        {
            Assert.That(blocks.Count, Is.EqualTo(blocks.Count));
            for (int i = 0; i < expected.Count; i++)
            {
                var seq = expected[i] as SequentialBlock;
                if (seq != null)
                {
                    Assert.That(blocks[i], Is.TypeOf<SequentialBlock>());
                    var actualSeq = (SequentialBlock)blocks[i];
                    for (int j = 0; j < seq.Instructions.Count; j++)
                    {
                        ILHelper.AssertILEqual(seq.Instructions[j], actualSeq.Instructions[j]);
                    }
                    continue;
                }
                var loop = expected[i] as LoopBlock;
                if (loop != null)
                {
                    Assert.That(blocks[i], Is.TypeOf<LoopBlock>());
                    var actualLoop = (LoopBlock)blocks[i];
                    AssertFlowEqual(loop.Children, actualLoop.Children);
                }
                var cond = expected[i] as ConditionalBlock;
                if (cond != null)
                {
                    Assert.That(blocks[i], Is.TypeOf<ConditionalBlock>());
                    var actualCond = (ConditionalBlock)blocks[i];
                    AssertFlowEqual(cond.LeftChildren, actualCond.LeftChildren);
                    AssertFlowEqual(cond.RightChildren, actualCond.RightChildren);
                }
            }
        }

        public static IBlock Sequential(params ILInstruction[] instructions)
        {
            var block = new SequentialBlock();
            foreach (var instr in instructions)
            {
                block.Add(instr);
            }
            return block;
        }

        public static IBlock Loop(params IBlock[] blocks)
        {
            return new LoopBlock(blocks.ToList());
        }

        public static IBlock Conditional(IReadOnlyList<IBlock> left, IReadOnlyList<IBlock> right)
        {
            return new ConditionalBlock(left.ToList(), right.ToList());
        }

        public static IReadOnlyList<IBlock> Blocks(params IBlock[] blocks)
        {
            return blocks;
        }
    }
}
