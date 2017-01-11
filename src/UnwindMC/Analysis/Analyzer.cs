using NDis86;
using NLog;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System;
using UnwindMC.Util;

namespace UnwindMC.Analysis
{
    public class Analyzer
    {
        private static readonly Logger Logger = LogManager.GetCurrentClassLogger();

        private readonly InstructionGraph _graph;
        private readonly SortedDictionary<ulong, Function> _functions = new SortedDictionary<ulong, Function>();
        private readonly SortedDictionary<ulong, JumpTable> _jumpTables = new SortedDictionary<ulong, JumpTable>(); 

        public Analyzer(ArraySegment<byte> textBytes, ulong pc)
        {
            _graph = new InstructionGraph(textBytes, pc);
        }

        public InstructionGraph Graph => _graph;

        public void Analyze()
        {
            AddExplicitCalls();
            ResolveFunctionBounds();
        }

        private void AddExplicitCalls()
        {
            Logger.Info("Adding explicit calls");
            foreach (var instr in _graph.Instructions)
            {
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

        private void ResolveFunctionBounds()
        {
            Logger.Info("Resolving function bounds");
            int index = 0;
            var keys = _functions.Keys;
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
                var allowedLinks = InstructionGraph.LinkType.Next | InstructionGraph.LinkType.Branch | InstructionGraph.LinkType.SwitchCaseJump;
                var visitedAllLinks = _graph.DFS(function.Address, allowedLinks, (instr, link) =>
                {
                    _graph.GetExtraData(instr.Offset).FunctionAddress = function.Address;
                    if (instr.Code == MnemonicCode.Iret)
                    {
                        return false;
                    }
                    AddNextLinks(instr);
                    AddExplicitBranches(instr);
                    AddSwitchCases(instr);
                    return true;
                });
                function.Status = visitedAllLinks
                    ? FunctionStatus.BoundsResolved
                    : FunctionStatus.BoundsNotResolvedIncompleteGraph;
            }
            Logger.Info("Done");
        }

        private void AddNextLinks(Instruction instr)
        {
            switch (instr.Code)
            {
                case MnemonicCode.Iret:
                case MnemonicCode.Ijmp:
                case MnemonicCode.Iint3:
                    break;
                default:
                    var next = _graph.GetNext(instr.Offset);
                    if (next != _graph.FirstAddressAfterCode)
                    {
                        _graph.AddLink(instr.Offset, next, InstructionGraph.LinkType.Next);
                    }
                    break;
            }
        }

        private void AddExplicitBranches(Instruction instr)
        {
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

        private void AddSwitchCases(Instruction instr)
        {
            if (instr.Code != MnemonicCode.Ijmp || instr.Operands[0].Type != OperandType.Memory)
            {
                return;
            }
            var address = instr.Operands[0].LValue.udword;
            JumpTable table;
            if (!_jumpTables.TryGetValue(address, out table))
            {
                table = new JumpTable(instr.Offset, address);
                ResolveJumpTable(table);
                _jumpTables.Add(address, table);
            }
            for (int i = table.FirstIndex; i < table.Count; i++)
            {
                _graph.AddLink(table.Reference, _graph.ReadUInt32(table.Address + (uint)(i * 4)), InstructionGraph.LinkType.SwitchCaseJump);
            }
        }

        private void ResolveJumpTable(JumpTable table)
        {
            if (table.Address >= _graph.FirstAddressAfterCode)
            {
                Logger.Warn("Jump table is not in the code segment");
                return;
            }

            Logger.Info("Resolving jump table tbl_{0:x8}", table.Address);
            OperandType idx = OperandType.None;
            OperandType lowByteIdx = OperandType.None;
            ulong indirectAddress = 0;
            var casesCountOption = Option<int>.None;
            _graph.ReverseDFS(table.Reference, InstructionGraph.LinkType.Next | InstructionGraph.LinkType.Branch, (instr, link) =>
            {
                // find out the jump index register
                if (idx == OperandType.None)
                {
                    idx = instr.Operands[0].Index;
                    lowByteIdx = RegisterHelper.GetLowByteRegisterFromDword(idx);
                    return true;
                }
                // update the main jump register if the jump is indirect
                if (indirectAddress == 0 && instr.Code == MnemonicCode.Imov && instr.Operands[0].Base == lowByteIdx)
                {
                    idx = instr.Operands[1].Base;
                    indirectAddress = instr.Operands[1].LValue.udword;
                    return true;
                }
                // search for cases count, can find it from code like cmp ecx, 0xb or mov ecx, 0xb
                // where the main jump register is tested against the max allowed case index
                // (in which cases there will be a conditional jump to the default branch)
                var tracker = new AssignmentTracker(_graph);
                var value = tracker.Find(instr.Offset, idx, (i, reg) =>
                    (i.Code == MnemonicCode.Icmp && i.Operands[0].Type == OperandType.Register && i.Operands[0].Base == reg) ||
                    (i.Code == MnemonicCode.Imov && i.Operands[0].Type == OperandType.Register && i.Operands[0].Base == reg && i.Operands[1].Type == OperandType.Immediate));
                casesCountOption = value.Map(v => v.ubyte + 1);
                return false;
            });

            if (!casesCountOption.HasValue)
            {
                Logger.Info("Done");
                return;
            }

            int casesCount = casesCountOption.Value;
            int jumpsCount;
            if (indirectAddress == 0)
            {
                jumpsCount = casesCount;
                casesCount = 0;
            }
            else
            {
                jumpsCount = _graph.GetBytes(indirectAddress, casesCount).Max() + 1;
            }

            uint offset = 0;
            for (int i = 0; i < jumpsCount; i++)
            {
                if (!_graph.AddJumpTableEntry(table.Address + offset))
                {
                    table.FirstIndex++;
                }
                _graph.AddJumpTableEntry(table.Address + offset);
                offset += 4;
            }
            table.Count = jumpsCount;
            while (casesCount >= 4)
            {
                _graph.AddJumpTableIndirectEntries(table.Address + offset, 4);
                casesCount -= 4;
                offset += 4;
            }
            if (casesCount > 0)
            {
                _graph.AddJumpTableIndirectEntries(table.Address + offset, casesCount);
                offset += (uint)casesCount;
            }
            _graph.Redisassemble(table.Address + offset);
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
                default: throw new ArgumentException();
            }
            return targetOffset;
        }

        public string DumpResults()
        {
            Logger.Info("Dumping results");
            var sb = new StringBuilder();
            int unresolvedInstructions = 0;
            int incompleteInstructions = 0;
            foreach (var instr in _graph.Instructions)
            {
                var address = _graph.GetExtraData(instr.Offset).FunctionAddress;
                string description;
                if (address == 0)
                {
                    if (instr.Code == MnemonicCode.Inop || instr.Assembly == "mov edi, edi" || instr.Assembly == "lea ecx, [ecx]")
                    {
                        description = "--------";
                    }
                    else if (instr.Code == MnemonicCode.Inone)
                    {
                        description = "jmptable";
                    }
                    else
                    {
                        description = "        ";
                        unresolvedInstructions++;
                    }
                }
                else if (_functions[address].Status == FunctionStatus.BoundsNotResolvedIncompleteGraph)
                {
                    description = "xxxxxxxx";
                    incompleteInstructions++;
                }
                else
                {
                    description = string.Format("{0:x8}", address);
                }
                sb.AppendFormat("{0} {1:x8} {2,20} {3}", description, instr.Offset, instr.Hex, instr.Assembly);
                sb.AppendLine();
            }
            var result = sb.ToString();
            Logger.Info("Done: {0} ({1:0%}) unresolved, {2} ({3:0%}) incomplete",
                unresolvedInstructions, (double)unresolvedInstructions / _graph.Instructions.Count,
                incompleteInstructions, (double)incompleteInstructions / _graph.Instructions.Count);
            return result;
        }
    }
}
