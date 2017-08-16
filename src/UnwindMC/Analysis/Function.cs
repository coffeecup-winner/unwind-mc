using System;
using System.Collections.Generic;
using UnwindMC.Analysis.Ast;
using UnwindMC.Analysis.Data;
using UnwindMC.Analysis.Flow;
using UnwindMC.Analysis.IL;
using UnwindMC.Emit;

namespace UnwindMC.Analysis
{
    public class Function
    {
        private List<IBlock> _blocks;
        private IReadOnlyList<Data.Type> _parameterTypes;
        private IReadOnlyList<Data.Type> _variableTypes;
        private ScopeNode _ast;
        private string _code;

        public Function(ulong address)
        {
            Address = address;
            Name = string.Format("sub_{0:x6}", address);
            Status = FunctionStatus.Created;
        }

        public ulong Address { get; }
        public string Name { get; private set; }
        public FunctionStatus Status { get; set; }
        public IReadOnlyList<IBlock> Blocks => _blocks;
        public IReadOnlyList<Data.Type> ParameterTypes => _parameterTypes;
        public IReadOnlyList<Data.Type> VariableTypes => _variableTypes;
        public ScopeNode Ast => _ast;
        public string Code => _code;
        public ILInstruction FirstInstruction
        {
            get
            {
                if (_blocks == null || _blocks.Count == 0)
                {
                    return null;
                }
                switch (_blocks[0])
                {
                    case SequentialBlock seq:
                        return seq.Instructions[0];
                    case WhileBlock loop:
                        return loop.Condition;
                    case ConditionalBlock cond:
                        return cond.Condition;
                    default: throw new InvalidOperationException("Unknown block type");
                }
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

        public void ResolveTypes()
        {
            if (Status != FunctionStatus.BodyResolved)
            {
                throw new InvalidOperationException("Cannot resolve arguments when body is not resolved");
            }
            var types = TypeResolver.ResolveTypes(_blocks);
            _parameterTypes = types.ParameterTypes;
            _variableTypes = types.VariableTypes;
            Status = FunctionStatus.ArgumentsResolved;
        }

        public void BuildAst()
        {
            if (Status != FunctionStatus.ArgumentsResolved)
            {
                throw new InvalidOperationException("Cannot build AST when arguments are not resolved");
            }
            _ast = new AstBuilder(_blocks, _parameterTypes, _variableTypes).BuildAst();
            Status = FunctionStatus.AstBuilt;
        }

        public void EmitSourceCode()
        {
            if (Status != FunctionStatus.AstBuilt)
            {
                throw new InvalidOperationException("Cannot emit source code when AST is not built");
            }
            var types = new Dictionary<string, Data.Type>();
            for (int i = 0; i < _parameterTypes.Count; i++)
            {
                types.Add("arg" + i, _parameterTypes[i]);
            }
            for (int i = 0; i < _variableTypes.Count; i++)
            {
                types.Add("var" + i, _variableTypes[i]);
            }
            _code = new CppEmitter(Name, types, _parameterTypes.Count, _ast).EmitSourceCode();
            Status = FunctionStatus.SourceCodeEmitted;
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
        SourceCodeEmitted,
    }
}
