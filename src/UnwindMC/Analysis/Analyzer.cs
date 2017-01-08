using NDis86;
using NLog;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace UnwindMC.Analysis
{
    public class Analyzer
    {
        private class InstructionExtraData
        {
            public ulong FunctionAddress { get; set; }
        }

        private static readonly Logger Logger = LogManager.GetCurrentClassLogger();

        private readonly InstructionGraph _graph;
        private readonly Dictionary<ulong, InstructionExtraData> _extraData;
        private readonly Dictionary<ulong, Function> _functions = new Dictionary<ulong, Function>();

        public Analyzer(IReadOnlyList<Instruction> instructions)
        {
            _graph = new InstructionGraph(instructions);
            _extraData = instructions.ToDictionary(i => i.Offset, i => new InstructionExtraData());
        }

        public InstructionGraph Graph => _graph;

        public string DumpResults()
        {
            Logger.Info("Dumping results");
            var sb = new StringBuilder();
            var notResolved = new string(' ', 8);
            var incomplete = new string('x', 8);
            int unresolvedInstructions = 0;
            int incompleteInstructions = 0;
            foreach (var instr in _graph.Instructions)
            {
                var address = _extraData[instr.Offset].FunctionAddress;
                string function;
                if (address == 0)
                {
                    function = notResolved;
                    if (instr.Code != MnemonicCode.Inop)
                    {
                        unresolvedInstructions++;
                    }
                }
                else if (_functions[address].Status == FunctionStatus.BoundsNotResolvedIncompleteGraph)
                {
                    function = incomplete;
                    incompleteInstructions++;
                }
                else
                {
                    function = string.Format("{0:x8}", address);
                }
                sb.AppendFormat("{0} {1:x8} {2,20} {3}", function, instr.Offset, instr.Hex, instr.Assembly);
                sb.AppendLine();
            }
            var result = sb.ToString();
            Logger.Info("Done: {0} ({1:0%}) unresolved, {2} ({3:0%}) incomplete",
                unresolvedInstructions, (double)unresolvedInstructions / _graph.Instructions.Count,
                incompleteInstructions, (double)incompleteInstructions / _graph.Instructions.Count);
            return result;
        }

        public void Analyze()
        {
            AddNextLinks();
            AddExplicitBranches();
            AddExplicitCalls();
            ResolveFunctionBounds();
        }

        private void AddNextLinks()
        {
            Logger.Info("Adding next links");
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
            Logger.Info("Done");
        }

        private void AddExplicitBranches()
        {
            Logger.Info("Adding explicit branches");
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
            Logger.Info("Done");
        }

        private void AddExplicitCalls()
        {
            Logger.Info("Adding explicit calls");
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
            Logger.Info("Done");
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

        private void ResolveFunctionBounds()
        {
            Logger.Info("Resolving function bounds");
            int index = 0;
            var keys = _functions.Keys.OrderBy(k => k);
            foreach (var key in keys)
            {
                var function = _functions[key];
                Logger.Debug("[{0}/{1}] sub_{2:x8}...", ++index, _functions.Count, function.Address);
                if (!_graph.Contains(function.Address))
                {
                    Logger.Warn("The specified address is not a valid start of instruction or was disassembled incorrectly");
                    function.Status = FunctionStatus.BoundsNotResolvedInvalidAddress;
                    continue;
                }
                var visitedAllLinks = _graph.DFS(function.Address, InstructionGraph.LinkType.Next | InstructionGraph.LinkType.Branch, instr =>
                {
                    _extraData[instr.Offset].FunctionAddress = function.Address;
                    return instr.Code != MnemonicCode.Iret;
                });
                function.Status = visitedAllLinks
                    ? FunctionStatus.BoundsResolved
                    : FunctionStatus.BoundsNotResolvedIncompleteGraph;
            }
            Logger.Info("Done");
        }
    }
}
