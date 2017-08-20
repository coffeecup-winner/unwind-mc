using NDis86;
using NLog;
using System.Collections.Generic;
using System.Linq;
using System;
using UnwindMC.Collections;
using UnwindMC.Util;

namespace UnwindMC.Analysis
{
    public class Analyzer
    {
        private static readonly Logger Logger = LogManager.GetCurrentClassLogger();

        private readonly InstructionGraph _graph;
        private readonly IImportResolver _importResolver;
        private readonly SortedDictionary<ulong, Function> _functions = new SortedDictionary<ulong, Function>();
        private readonly SortedDictionary<ulong, JumpTable> _jumpTables = new SortedDictionary<ulong, JumpTable>(); 

        public Analyzer(ArraySegment<byte> textBytes, ulong pc, IImportResolver importResolver)
        {
            _graph = new InstructionGraph(textBytes, pc);
            _importResolver = importResolver;
        }

        public InstructionGraph Graph => _graph;
        public IDictionary<ulong, Function> Functions => _functions;

        public void Analyze()
        {
            AddExplicitCalls();
            ResolveFunctionBounds();
            ResolveExternalFunctionCalls();
        }

        public void AddFunction(ulong address)
        {
            _functions.Add(address, new Function(address));
        }

        private void AddExplicitCalls()
        {
            Logger.Info("Adding explicit calls");
            foreach (var instr in _graph.Instructions)
            {
                if (instr.Code == MnemonicCode.Icall && instr.Operands[0].Type == OperandType.ImmediateBranch)
                {
                    var targetAddress = instr.GetTargetAddress();
                    _graph.AddLink(instr.Offset, targetAddress, InstructionGraph.LinkType.Call);
                    if (!_functions.ContainsKey(targetAddress))
                    {
                        _functions.Add(targetAddress, new Function(targetAddress));
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
                if (!_graph.InBounds(function.Address))
                {
                    Logger.Warn("The specified address is outside of code segment");
                    function.Status = FunctionStatus.BoundsNotResolvedInvalidAddress;
                    continue;
                }
                if (!_graph.Contains(function.Address))
                {
                    Logger.Debug("The specified address is not a valid start of instruction, re-disassembling");
                    _graph.Redisassemble(function.Address);
                }
                var visitedAllLinks = _graph
                    .WithEdgeFilter(e => (e.Type & InstructionGraph.LinkType.Next | InstructionGraph.LinkType.Branch | InstructionGraph.LinkType.SwitchCaseJump) != 0)
                    .DFS(function.Address, (instr, link) =>
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

        private void ResolveExternalFunctionCalls()
        {
            Logger.Info("Resolving external calls");
            foreach (var instr in _graph.Instructions)
            {
                var import = TryGetImportAddress(instr, 1)
                    .OrElse(() => TryGetImportAddress(instr, 0))
                    .Map(_importResolver.GetImportName);
                if (import.TryGet(out var name))
                {
                    _graph.GetExtraData(instr.Offset).ImportName = name;
                }
            }
            Logger.Info("Done");
        }

        private Option<ulong> TryGetImportAddress(Instruction instr, int index)
        {
            if (instr.Operands.Count > index && instr.Operands[index].Type == OperandType.Memory &&
                instr.Operands[index].Base == OperandType.None && instr.Operands[index].Index == OperandType.None &&
                _importResolver.IsImportAddress(instr.Operands[index].LValue.udword))
            {
                return Option.Some(instr.Operands[index].LValue.uqword);
            }
            return Option<ulong>.None;
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
                        _graph.AddLink(instr.Offset, instr.GetTargetAddress(), InstructionGraph.LinkType.Branch);
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
            if (!_jumpTables.TryGetValue(address, out var table))
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
            _graph
                .WithEdgeFilter(e => (e.Type & InstructionGraph.LinkType.Next | InstructionGraph.LinkType.Branch) != 0)
                .ReverseEdges()
                .DFS(table.Reference, (instr, link) =>
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
                // search for the jump to the default case
                if (!(instr.Code == MnemonicCode.Ija && instr.Operands[0].Type == OperandType.ImmediateBranch))
                {
                    return true;
                }
                // search for cases count, can find it from code like cmp ecx, 0xb
                // the jump register is irrelevant since it must be the closest one to ja
                var tracker = new AssignmentTracker(_graph);
                var value = tracker.Find(instr.Offset, idx, (i, _) =>
                    i.Code == MnemonicCode.Icmp && i.Operands[0].Type == OperandType.Register);
                casesCountOption = value.Map(v => v.ubyte + 1);
                return false;
            });

            if (!casesCountOption.TryGet(out var casesCount))
            {
                Logger.Info("Done");
                return;
            }

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
    }
}
