using NDis86;
using System.Collections.Generic;

namespace UnwindMC.Analysis
{
    public class Analyzer
    {
        private readonly InstructionGraph _graph;
        private readonly Dictionary<ulong, Function> _functions = new Dictionary<ulong, Function>();

        public Analyzer(IReadOnlyList<Instruction> instructions)
        {
            _graph = new InstructionGraph(instructions);
        }

        public InstructionGraph Graph => _graph;

        public void Analyze()
        {
            AddNextLinks();
            AddExplicitBranches();
            AddExplicitCalls();
        }

        private void AddNextLinks()
        {
            for (int i = 0; i < _graph.Instructions.Count - 1; i++)
            {
                var instr = _graph.Instructions[i];
                switch (instr.Code)
                {
                    case MnemonicCode.Iret:
                    case MnemonicCode.Ijmp:
                    case MnemonicCode.Iint3:
                        break;
                    default:
                        _graph.AddLink(instr.Offset, _graph.Instructions[i + 1].Offset, InstructionGraph.LinkType.Next);
                        break;
                }
            }
        }

        private void AddExplicitBranches()
        {
            for (int i = 0; i < _graph.Instructions.Count; i++)
            {
                var instr = _graph.Instructions[i];
                switch (instr.Code)
                {
                    case MnemonicCode.Ija:
                    case MnemonicCode.Ijae:
                    case MnemonicCode.Ijb:
                    case MnemonicCode.Ijbe:
                    case MnemonicCode.Ijcxz:
                    case MnemonicCode.Ijecxz:
                    case MnemonicCode.Ijg:
                    case MnemonicCode.Ijge:
                    case MnemonicCode.Ijl:
                    case MnemonicCode.Ijle:
                    case MnemonicCode.Ijmp:
                    case MnemonicCode.Ijno:
                    case MnemonicCode.Ijnp:
                    case MnemonicCode.Ijns:
                    case MnemonicCode.Ijnz:
                    case MnemonicCode.Ijo:
                    case MnemonicCode.Ijp:
                    case MnemonicCode.Ijrcxz:
                    case MnemonicCode.Ijs:
                    case MnemonicCode.Ijz:
                        if (instr.Operands[0].Type == OperandType.ImmediateBranch)
                        {
                            _graph.AddLink(instr.Offset, GetRelativeTargetOffset(instr), InstructionGraph.LinkType.Branch);
                        }
                        break;
                }
            }
        }

        private void AddExplicitCalls()
        {
            for (int i = 0; i < _graph.Instructions.Count; i++)
            {
                var instr = _graph.Instructions[i];
                if (instr.Code == MnemonicCode.Icall && instr.Operands[0].Type == OperandType.ImmediateBranch)
                {
                    var targetOffset = GetRelativeTargetOffset(instr);
                    _graph.AddLink(instr.Offset, targetOffset, InstructionGraph.LinkType.Call);
                    if (!_functions.ContainsKey(targetOffset))
                    {
                        _functions.Add(targetOffset, new Function(targetOffset));
                    }
                }
            }
        }

        private static ulong GetRelativeTargetOffset(Instruction instr)
        {
            var targetOffset = instr.Offset + instr.Length;
            switch (instr.Operands[0].Size)
            {
                case 8: targetOffset = (ulong)((long)targetOffset + instr.Operands[0].LValue.@sbyte); break;
                case 16: targetOffset = (ulong)((long)targetOffset + instr.Operands[0].LValue.sword); break;
                case 32: targetOffset = (ulong)((long)targetOffset + instr.Operands[0].LValue.sdword); break;
                case 64: targetOffset = (ulong)((long)targetOffset + instr.Operands[0].LValue.sqword); break;
            }
            return targetOffset;
        }
    }
}
