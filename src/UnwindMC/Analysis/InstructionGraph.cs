using NDis86;
using NLog;
using System;
using System.Collections.Generic;
using System.Text;
using UnwindMC.Collections;
using UnwindMC.Util;

namespace UnwindMC.Analysis
{
    public class InstructionGraph : IGraph<ulong, Instruction, InstructionGraph.Link>
    {
        private readonly Logger Logger = LogManager.GetCurrentClassLogger();

        public class Link
        {
            public Link(ulong address, ulong targetAddress, LinkType type)
            {
                Address = address;
                TargetAddress = targetAddress;
                Type = type;
            }

            public ulong Address { get; }
            public ulong TargetAddress { get; }
            public LinkType Type { get; }
        }

        public class ExtraData
        {
            public ulong FunctionAddress { get; set; }
            public string ImportName { get; set; }
            public bool IsProtected { get; set; }
        }

        [Flags]
        public enum LinkType
        {
            Next = 0x01,
            Branch = 0x02,
            Call = 0x04,
            SwitchCaseJump = 0x08,
        }

        private readonly Disassembler _disassembler;
        private readonly ArraySegment<byte> _bytes;
        private readonly ulong _firstAddress;
        private readonly ulong _firstAddressAfterCode;
        private readonly SortedDictionary<ulong, Instruction> _instructions = new SortedDictionary<ulong, Instruction>();
        private readonly Dictionary<ulong, ExtraData> _extraData = new Dictionary<ulong, ExtraData>();
        private readonly Dictionary<ulong, List<Link>> _instructionLinks = new Dictionary<ulong, List<Link>>();
        private readonly Dictionary<ulong, List<Link>> _reverseLinks = new Dictionary<ulong, List<Link>>();
        private readonly bool _isReversed;
        private readonly Func<Link, bool> _edgePredicate;

        public InstructionGraph(ArraySegment<byte> bytes, ulong pc)
        {
            _disassembler = new Disassembler(DisassemblyMode.Mode32Bit, DisassemblySyntax.Intel, pc);
            Logger.Info("Disassembling machine code");
            var instructions = _disassembler.Disassemble(bytes.Array, bytes.Offset, bytes.Count, withHex: true, withAssembly: true);
            Logger.Info("Done");
            _bytes = bytes;
            _firstAddress = pc;
            var lastInstruction = instructions[instructions.Count - 1];
            _firstAddressAfterCode = lastInstruction.Offset + lastInstruction.Length;
            foreach (var instr in instructions)
            {
                _instructions.Add(instr.Offset, instr);
            }
            _isReversed = false;
            _edgePredicate = _ => true;
        }

        private InstructionGraph(Disassembler disassembler, ArraySegment<byte> bytes, ulong firstAddress, ulong firstAddressAfterCode,
            SortedDictionary<ulong, Instruction> instructions, Dictionary<ulong, ExtraData> extraData, Dictionary<ulong, List<Link>> instructionLinks,
            Dictionary<ulong, List<Link>> reverseLinks, bool isReversed, Func<Link, bool> edgePredicate)
        {
            _disassembler = disassembler;
            _bytes = bytes;
            _firstAddress = firstAddress;
            _firstAddressAfterCode = firstAddressAfterCode;
            _instructions = instructions;
            _extraData = extraData;
            _instructionLinks = instructionLinks;
            _reverseLinks = reverseLinks;
            _isReversed = isReversed;
            _edgePredicate = edgePredicate;
        }

        public ICollection<Instruction> Instructions => _instructions.Values;
        public ulong FirstAddressAfterCode => _firstAddressAfterCode;

        public bool Contains(ulong address)
        {
            return _instructions.ContainsKey(address);
        }

        public Instruction GetVertex(ulong vertexId)
        {
            return _instructions[vertexId];
        }

        public bool Contains(Instruction vertex)
        {
            return _instructions.ContainsKey(vertex.Offset);
        }

        public IEnumerable<Either<(ulong vertex, Link edge), string>> GetAdjacent(ulong vertexId)
        {
            if (!(_isReversed ? _reverseLinks : _instructionLinks).TryGetValue(vertexId, out var links))
            {
                yield return "DFS: Couldn't find links for " + GetVertex(vertexId);
                yield break;
            }
            foreach (var link in links)
            {
                if (!_edgePredicate(link))
                {
                    continue;
                }
                var linkAddress = _isReversed ? link.Address : link.TargetAddress;
                yield return InBounds(linkAddress)
                    ? (Either<(ulong vertex, Link edge), string>)(linkAddress, link)
                    : "DFS: Jump outside of code section";
            }
        }

        public IGraph<ulong, Instruction, Link> GetSubgraph(ISet<Instruction> subgraph)
        {
            throw new NotSupportedException();
        }

        public IGraph<ulong, Instruction, Link> WithEdgeFilter(Func<Link, bool> predicate)
        {
            return new InstructionGraph(_disassembler, _bytes, _firstAddress, _firstAddressAfterCode, _instructions,
                _extraData, _instructionLinks, _reverseLinks, _isReversed, predicate);
        }

        public IGraph<ulong, Instruction, Link> ReverseEdges()
        {
            return new InstructionGraph(_disassembler, _bytes, _firstAddress, _firstAddressAfterCode, _instructions,
                _extraData, _instructionLinks, _reverseLinks, !_isReversed, _edgePredicate);
        }

        public bool InBounds(ulong address)
        {
            return address >= _firstAddress && address < _firstAddressAfterCode;
        }

        public ulong GetNext(ulong address)
        {
            do
            {
                address++;
            }
            while (!_instructions.ContainsKey(address) && address < _firstAddressAfterCode);
            return address;
        }

        public int GetInValue(ulong address)
        {
            return _reverseLinks[address].Count;
        }

        public int GetOutValue(ulong address)
        {
            return _instructionLinks[address].Count;
        }

        public ArraySegment<byte> GetBytes(ulong address, int size)
        {
            return new ArraySegment<byte>(_bytes.Array, ToByteArrayIndex(address), size);
        }

        public ExtraData GetExtraData(ulong address)
        {
            if (!_extraData.TryGetValue(address, out var data))
            {
                data = new ExtraData();
                _extraData.Add(address, data);
            }
            return data;
        }

        public void AddLink(ulong offset, ulong targetOffset, LinkType type)
        {
            var link = new Link(offset, targetOffset, type);

            if (!_instructionLinks.TryGetValue(offset, out var links))
            {
                links = new List<Link>();
                _instructionLinks[offset] = links;
            }
            links.Add(link);

            if (!_reverseLinks.TryGetValue(targetOffset, out links))
            {
                links = new List<Link>();
                _reverseLinks[targetOffset] = links;
            }
            links.Add(link);
        }

        public void ClearLinks()
        {
            _instructionLinks.Clear();
            _reverseLinks.Clear();
        }

        public bool AddJumpTableEntry(ulong address)
        {
            var data = ReadUInt32(address);
            if (!InBounds(data))
            {
                return false;
            }
            MarkDataBytes(address, 4, string.Format("dd {0:x8}", data));
            return true;
        }

        public void AddJumpTableIndirectEntries(ulong address, int count)
        {
            var sb = new StringBuilder();
            sb.Append("db");
            int start = ToByteArrayIndex(address);
            for (int i = 0; i < count; i++)
            {
                sb.AppendFormat(" {0:x2}", _bytes.Array[start + i]);
            }
            MarkDataBytes(address, count, sb.ToString());
        }

        private void MarkDataBytes(ulong address, int length, string dataDisplayText)
        {
            int size;
            // check if there is an instruction at address, otherwise split an existing one
            if (_instructions.TryGetValue(address, out var instr))
            {
                size = instr.Length;
            }
            else
            {
                instr = SplitInstructionAt(address);
                size = instr.Length - (int)(address - instr.Offset);
            }
            // write a pseudo instruction
            var sb = new StringBuilder();
            for (uint i = 0; i < length; i++)
            {
                sb.AppendFormat("{0:x2}", _bytes.Array[ToByteArrayIndex(address + i)]);
            }
            _instructions[address] = new Instruction(address, MnemonicCode.Inone, (byte)length, sb.ToString(), dataDisplayText,
                new Operand[0], 0, OperandType.None, false, false, 0, 0, 0, 0, 0);
            GetExtraData(address).IsProtected = true;
            // remove old instructions
            while (size < length)
            {
                instr = _instructions[address + (ulong)size];
                _instructions.Remove(address + (ulong)size);
                size += instr.Length;
            }
            // if there are bytes left from the last removed instruction, re-disassemble them
            if (size != length)
            {
                _disassembler.SetPC(address + (uint)length);
                var instructions = _disassembler.Disassemble(_bytes.Array, ToByteArrayIndex(address) + length, size - length, withHex: true, withAssembly: true);
                foreach (var newInstr in instructions)
                {
                    _instructions[newInstr.Offset] = newInstr;
                }
            }
        }

        private Instruction SplitInstructionAt(ulong address)
        {
            var instrAddress = address;
            Instruction oldInstr;
            while (!_instructions.TryGetValue(--instrAddress, out oldInstr)) { }
            // Split the old instruction and re-disassemble its first part
            int extraLength = (int)(address - instrAddress);
            _disassembler.SetPC(instrAddress);
            var instructions = _disassembler.Disassemble(_bytes.Array, ToByteArrayIndex(instrAddress), extraLength, withHex: true, withAssembly: true);
            foreach (var newInstr in instructions)
            {
                _instructions[newInstr.Offset] = newInstr;
            }
            return oldInstr;
        }

        public ulong ReadUInt32(ulong address) =>
            _bytes.Array.ReadUInt32(ToByteArrayIndex(address));

        public void Redisassemble(ulong address)
        {
            // This function will fix any instructions that were incorrectly disassembled because of the data block that was treated as code
            if (!_instructions.ContainsKey(address))
            {
                SplitInstructionAt(address);
            }
            _disassembler.SetPC(address);
            const int maxDisassembleLength = 0x100;
            int disassembleLength = Math.Min(maxDisassembleLength, (int)(_firstAddressAfterCode - address));
            for (int j = 0; j < disassembleLength; j++)
            {
                if (_extraData.TryGetValue(address + (uint)j, out var data) && data.IsProtected)
                {
                    disassembleLength = j;
                }
            }
            var instructions = _disassembler.Disassemble(_bytes.Array, ToByteArrayIndex(address), disassembleLength, withHex: true, withAssembly: true);
            bool fixedInstructions = false;
            int i;
            // Take 1 instruction less since it can be incorrectly disassembled (partial data)
            for (i = 0; i < instructions.Count - 1; i++)
            {
                var newInstr = instructions[i];
                if (_instructions.TryGetValue(newInstr.Offset, out var oldInstr) && oldInstr.Length == newInstr.Length)
                {
                    if (!fixedInstructions)
                    {
                        continue;
                    }
                    else
                    {
                        break;
                    }
                }
                _instructions[newInstr.Offset] = newInstr;
                for (uint j = 1; j < newInstr.Length; j++)
                {
                    if (_instructions.ContainsKey(newInstr.Offset + j))
                    {
                        _instructions.Remove(newInstr.Offset + j);
                    }
                }
                fixedInstructions = true;
            }
            if (fixedInstructions && i == instructions.Count - 1)
            {
                throw new Exception("FIXME: extra disassemble size was too small");
            }
        }

        private int ToByteArrayIndex(ulong address)
        {
            return _bytes.Offset + (int)(address - _firstAddress);
        }
    }
}
