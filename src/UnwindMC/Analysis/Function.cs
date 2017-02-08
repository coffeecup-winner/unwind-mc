using System;
using System.Collections.Generic;
using UnwindMC.Analysis.Ast;
using UnwindMC.Analysis.Data;
using UnwindMC.Analysis.Flow;
using UnwindMC.Analysis.IL;

namespace UnwindMC.Analysis
{
    public class Function
    {
        private List<IBlock> _blocks;
        private IReadOnlyDictionary<ILOperand, Data.Type> _arguments;
        private ScopeNode _ast;

        public Function(ulong address)
        {
            Address = address;
            Status = FunctionStatus.Created;
        }

        public ulong Address { get; }
        public FunctionStatus Status { get; set; }
        public IReadOnlyList<IBlock> Blocks => _blocks;
        public IReadOnlyDictionary<ILOperand, Data.Type> Arguments => _arguments;
        public ScopeNode Ast => _ast;
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

        public void ResolveArguments()
        {
            if (Status != FunctionStatus.BodyResolved)
            {
                throw new InvalidOperationException("Cannot resolve arguments when body is not resolved");
            }
            _arguments = TypeResolver.ResolveFunctionArguments(_blocks);
            Status = FunctionStatus.ArgumentsResolved;
        }

        public void BuildAst()
        {
            if (Status != FunctionStatus.ArgumentsResolved)
            {
                throw new InvalidOperationException("Cannot build AST when arguments are not resolved");
            }
            _ast = new AstBuilder(_blocks, _arguments).BuildAst();
            Status = FunctionStatus.AstBuilt;
        }
    }

    public enum FunctionStatus
    {
        Created,
        BoundsResolved,
        BoundsNotResolvedInvalidAddress,
        BoundsNotResolvedIncompleteGraph,
        BodyResolved,
        ArgumentsResolved,
        AstBuilt,
    }
}
