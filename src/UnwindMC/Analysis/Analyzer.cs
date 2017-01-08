using NDis86;
using NLog;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System;

namespace UnwindMC.Analysis
{
    public class Analyzer
    {
        private class InstructionExtraData
        {
            public ulong FunctionAddress { get; set; }
        }

        private static readonly Logger Logger = LogManager.GetCurrentClassLogger();

        private readonly Disassembler _disassembler;
        private readonly InstructionGraph _graph;
        private readonly SortedDictionary<ulong, InstructionExtraData> _extraData = new SortedDictionary<ulong, InstructionExtraData>();
        private readonly SortedDictionary<ulong, Function> _functions = new SortedDictionary<ulong, Function>();
        private readonly SortedDictionary<ulong, JumpTable> _jumpTables = new SortedDictionary<ulong, JumpTable>(); 

        public Analyzer(ArraySegment<byte> textBytes, ulong pc)
        {
            _disassembler = new Disassembler(syntax: DisassemblySyntax.Intel, pc: pc);
            Logger.Info("Disassembling machine code");
            var instructions = _disassembler.Disassemble(textBytes.Array, textBytes.Offset, textBytes.Count, withHex: true, withAssembly: true);
            Logger.Info("Done");
            _graph = new InstructionGraph(_disassembler, instructions, textBytes, pc);
        }

        public void Analyze()
        {
            AddExplicitCalls();
            InitializeExtraData();
            ResolveFunctionBounds();

            AddMemoryJumps();
            ResolveJumpTables();
            UpdateExtraData();
        }

        private void AddMemoryJumps()
        {
            Logger.Info("Adding memory jumps");
            foreach (var instr in _graph.Instructions)
            {
                switch (instr.Code)
                {
                    case MnemonicCode.Ijmp:
                        if (instr.Operands[0].Type == OperandType.Memory)
                        {
                            var address = GetTargetAddress(instr);
                            _graph.AddLink(instr.Offset, address, InstructionGraph.LinkType.MemoryJump,
                                instr.Operands[0].Base, instr.Operands[0].Index);
                            if (!_jumpTables.ContainsKey(address))
                            {
                                _jumpTables.Add(address, new JumpTable(address));
                            }
                        }
                        break;
                }
            }
            Logger.Info("Done");
        }

        private void ResolveJumpTables()
        {
            Logger.Info("Resolving jump tables");
            int index = 0;
            var keys = _jumpTables.Keys;
            foreach (var key in keys)
            {
                var table = _jumpTables[key];
                Logger.Debug("[{0}/{1}] tbl_{2:x8}...", ++index, _jumpTables.Count, table.Address);
                if (table.Address >= _graph.FirstAddressAfterCode)
                {
                    Logger.Warn("Jump table is not in the code segment");
                    continue;
                }
                uint offset = 0;
                while (!_graph.AddJumpTableEntry(table.Address + offset))
                {
                    table.FirstIndex++;
                    offset += 4;
                }
                while (_graph.AddJumpTableEntry(table.Address + offset))
                {
                    offset += 4;
                }
                table.MaxIndex = (int)offset / 4 - 1;
            }
            Logger.Info("Done");
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

        private void InitializeExtraData()
        {
            foreach (var instr in _graph.Instructions)
            {
                _extraData.Add(instr.Offset, new InstructionExtraData());
            }
        }

        private void UpdateExtraData()
        {
            var deleted = new HashSet<ulong>();
            var missing = new HashSet<ulong>();
            var addresses = new HashSet<ulong>(_graph.Instructions.Select(i => i.Offset));
            foreach (var address in addresses)
            {
                if (!_extraData.ContainsKey(address))
                {
                    missing.Add(address);
                }
            }
            foreach (var address in _extraData.Keys)
            {
                if (!addresses.Contains(address))
                {
                    deleted.Add(address);
                }
            }
            foreach (var address in deleted)
            {
                _extraData.Remove(address);
            }
            foreach (var address in missing)
            {
                _extraData.Add(address, new InstructionExtraData());
            }
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
                var hasUnresolvedMemoryJumps = false;
                var visitedAllLinks = _graph.DFS(function.Address, InstructionGraph.LinkType.Next | InstructionGraph.LinkType.Branch | InstructionGraph.LinkType.MemoryJump, (instr, link) =>
                {
                    _extraData[instr.Offset].FunctionAddress = function.Address;
                    if (instr.Code == MnemonicCode.Iret)
                    {
                        return false;
                    }
                    if (link.Type == InstructionGraph.LinkType.MemoryJump)
                    {
                        if (!link.IsResolved)
                        {
                            hasUnresolvedMemoryJumps = true;
                        }
                    }
                    else
                    {
                        AddNextLinks(instr);
                        AddExplicitBranches(instr);
                    }
                    return true;
                });
                function.Status = !hasUnresolvedMemoryJumps && visitedAllLinks
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

        private static ulong GetTargetAddress(Instruction instr)
        {
            switch (instr.Operands[0].Size)
            {
                case 8: return instr.Operands[0].LValue.ubyte;
                case 16: return instr.Operands[0].LValue.uword;
                case 32: return instr.Operands[0].LValue.udword;
                case 64: return instr.Operands[0].LValue.uqword;
                default: throw new ArgumentException();
            }
        }

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
    }
}
