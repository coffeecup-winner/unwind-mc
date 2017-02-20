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
                var whileLoop = expected[i] as WhileBlock;
                if (whileLoop != null)
                {
                    Assert.That(blocks[i], Is.TypeOf<WhileBlock>());
                    var actualWhileLoop = (WhileBlock)blocks[i];
                    ILHelper.AssertILEqual(whileLoop.Condition, actualWhileLoop.Condition);
                    AssertFlowEqual(whileLoop.Children, actualWhileLoop.Children);
                }
                var doWhileLoop = expected[i] as DoWhileBlock;
                if (doWhileLoop != null)
                {
                    Assert.That(blocks[i], Is.TypeOf<DoWhileBlock>());
                    var actualDoWhileLoop = (DoWhileBlock)blocks[i];
                    ILHelper.AssertILEqual(doWhileLoop.Condition, actualDoWhileLoop.Condition);
                    AssertFlowEqual(doWhileLoop.Children, actualDoWhileLoop.Children);
                }
                var cond = expected[i] as ConditionalBlock;
                if (cond != null)
                {
                    Assert.That(blocks[i], Is.TypeOf<ConditionalBlock>());
                    var actualCond = (ConditionalBlock)blocks[i];
                    ILHelper.AssertILEqual(cond.Condition, actualCond.Condition);
                    AssertFlowEqual(cond.TrueBranch, actualCond.TrueBranch);
                    AssertFlowEqual(cond.FalseBranch, actualCond.FalseBranch);
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

        public static IBlock While(ILInstruction condition, params IBlock[] blocks)
        {
            return new WhileBlock(condition, blocks.ToList());
        }

        public static IBlock DoWhile(ILInstruction condition, params IBlock[] blocks)
        {
            return new DoWhileBlock(condition, blocks.ToList());
        }

        public static IBlock Conditional(ILInstruction condition, IReadOnlyList<IBlock> trueBranch, IReadOnlyList<IBlock> falseBranch)
        {
            return new ConditionalBlock(condition, trueBranch.ToList(), falseBranch.ToList());
        }

        public static IReadOnlyList<IBlock> Blocks(params IBlock[] blocks)
        {
            return blocks;
        }
    }
}
