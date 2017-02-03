using System;
using System.Collections.Generic;
using UnwindMC.Analysis.Flow;
using UnwindMC.Analysis.IL;

namespace UnwindMC.Analysis
{
    public class Function
    {
        private List<IBlock> _blocks;

        public Function(ulong address)
        {
            Address = address;
            Status = FunctionStatus.Created;
        }

        public ulong Address { get; }
        public FunctionStatus Status { get; set; }
        public IReadOnlyList<IBlock> Blocks => _blocks;
        public ILInstruction FirstInstruction
        {
            get
            {
                if (_blocks == null || _blocks.Count == 0)
                {
                    return null;
                }
                var seq = _blocks[0] as SequentialBlock;
                if (seq != null)
                {
                    return seq.Instructions[0];
                }
                var loop = _blocks[0] as LoopBlock;
                if (loop != null)
                {
                    return loop.Condition;
                }
                var cond = _blocks[0] as ConditionalBlock;
                if (cond != null)
                {
                    return cond.Condition;
                }
                throw new InvalidOperationException("Unknown block type");
            }
        }

        public void ResolveBody(InstructionGraph graph)
        {
            if (Status != FunctionStatus.BoundsResolved)
            {
                throw new InvalidOperationException("Cannot resolve function body when bounds are not resolved");
            }
            _blocks = FlowAnalyzer.Analyze(ILDecompiler.Decompile(graph, Address));
            Status = FunctionStatus.BodyResolved;
        }
    }

    public enum FunctionStatus
    {
        Created,
        BoundsResolved,
        BoundsNotResolvedInvalidAddress,
        BoundsNotResolvedIncompleteGraph,
        BodyResolved,
    }
}
