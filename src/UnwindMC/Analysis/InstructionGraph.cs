using NDis86;
using NLog;
using System;
using System.Collections.Generic;
using System.Text;

namespace UnwindMC.Analysis
{
    public class InstructionGraph
    {
        private readonly Logger Logger = LogManager.GetCurrentClassLogger();

        public class Link
        {
            public Link(ulong offset, ulong targetOffset, LinkType type, OperandType @base, OperandType index)
            {
                Offset = offset;
                TargetOffset = targetOffset;
                Type = type;
                Base = @base;
                Index = index;
            }

            public ulong Offset { get; }
            public ulong TargetOffset { get; }
            public LinkType Type { get; }
            public OperandType Base { get; }
            public OperandType Index { get; }
            public bool IsResolved { get; set; }
        }

        [Flags]
        public enum LinkType
        {
            Next = 0x01,
            Branch = 0x02,
            Call = 0x04,
            MemoryJump = 0x08,
        }

        private readonly Disassembler _disassembler;
        private readonly ArraySegment<byte> _bytes;
        private readonly ulong _firstAddress;
        private readonly ulong _firstAddressAfterCode;
        private readonly SortedDictionary<ulong, Instruction> _instructions = new SortedDictionary<ulong, Instruction>();
        private readonly Dictionary<ulong, List<Link>> _instructionLinks = new Dictionary<ulong, List<Link>>();
        private readonly Dictionary<ulong, List<Link>> _reverseLinks = new Dictionary<ulong, List<Link>>();

        public InstructionGraph(Disassembler disassembler, IReadOnlyList<Instruction> instructions, ArraySegment<byte> bytes, ulong pc)
        {
            _disassembler = disassembler;
            _bytes = bytes;
            _firstAddress = pc;
            var lastInstruction = instructions[instructions.Count - 1];
            _firstAddressAfterCode = lastInstruction.Offset + lastInstruction.Length;
            foreach (var instr in instructions)
            {
                _instructions.Add(instr.Offset, instr);
            }
        }

        public ICollection<Instruction> Instructions => _instructions.Values;
        public ulong FirstAddressAfterCode => _firstAddressAfterCode;

        public bool Contains(ulong offset)
        {
            return _instructions.ContainsKey(offset);
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

        public ArraySegment<byte> GetBytes(ulong address, int size)
        {
            return new ArraySegment<byte>(_bytes.Array, ToByteArrayIndex(address), size);
        }

        public void AddLink(ulong offset, ulong targetOffset, LinkType type,
            OperandType @base = OperandType.None, OperandType index = OperandType.None)
        {
            var link = new Link(offset, targetOffset, type, @base, index);

            List<Link> links;
            if (!_instructionLinks.TryGetValue(offset, out links))
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
            if (data < _firstAddress || data >= _firstAddressAfterCode)
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
            Instruction instr;
            // check if there is an instruction at address, otherwise split an existing one
            if (_instructions.TryGetValue(address, out instr))
            {
                size = instr.Length;
            }
            else
            {
                var instrAddress = address;
                while (!_instructions.TryGetValue(--instrAddress, out instr)) { }
                // Split the old instruction and re-disassemble its first part
                int extraLength = (int)(address - instrAddress);
                _disassembler.SetPC(instrAddress);
                var instructions = _disassembler.Disassemble(_bytes.Array, ToByteArrayIndex(instrAddress), extraLength, withHex: true, withAssembly: true);
                foreach (var newInstr in instructions)
                {
                    _instructions[newInstr.Offset] = newInstr;
                }
                size = instr.Length - extraLength;
            }
            // write a pseudo instruction
            _instructions[address] = new Instruction(address, MnemonicCode.Inone, (byte)length, null, dataDisplayText,
                new Operand[0], 0, OperandType.None, false, false, 0, 0, 0, 0, 0);
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

        public void Redisassemble(ulong address)
        {
            // This will fix any instructions that were incorrectly disassembled because of the data block that was treated as code
            _disassembler.SetPC(address);
            const int maxDisassembleLength = 0x100;
            int disassembleLength = Math.Min(maxDisassembleLength, (int)(_firstAddressAfterCode - address));
            var instructions = _disassembler.Disassemble(_bytes.Array, ToByteArrayIndex(address), disassembleLength, withHex: true, withAssembly: true);
            bool fixedInstructions = false;
            int i;
            // Take 1 instruction less since it can be incorrectly disassembled (partial data)
            for (i = 0; i < instructions.Count - 1; i++)
            {
                var newInstr = instructions[i];
                Instruction oldInstr;
                if (_instructions.TryGetValue(newInstr.Offset, out oldInstr) && oldInstr.Length == newInstr.Length)
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
            if (fixedInstructions && i == instructions.Count)
            {
                throw new Exception("FIXME: extra disassemble size was too small");
            }
        }

        private uint ReadUInt32(ulong address)
        {
            uint result = 0;
            int baseIndex = ToByteArrayIndex(address);
            for (int i = 3; i >= 0; i--)
            {
                result <<= 8;
                result += _bytes.Array[baseIndex + i];
            }
            return result;
        }

        public bool DFS(ulong offset, LinkType type, Func<Instruction, Link, bool> process)
        {
            return DFS(offset, type, process, reverse: false);
        }

        public bool ReverseDFS(ulong offset, LinkType type, Func<Instruction, Link, bool> process)
        {
            return DFS(offset, type, process, reverse: true);
        }

        private bool DFS(ulong offset, LinkType type, Func<Instruction, Link, bool> process, bool reverse)
        {
            var visited = new HashSet<ulong>();
            var stack = new Stack<Tuple<ulong, Link>>();
            stack.Push(Tuple.Create(offset, new Link(ulong.MaxValue, offset, LinkType.Call, OperandType.None, OperandType.None)));
            var visitedAllLinks = true;
            while (stack.Count > 0)
            {
                var current = stack.Pop();
                var instr = _instructions[current.Item1];
                visited.Add(current.Item1);
                if (!process(instr, current.Item2))
                {
                    continue;
                }

                List<Link> links;
                if (!(reverse ? _reverseLinks : _instructionLinks).TryGetValue(current.Item1, out links))
                {
                    Logger.Warn("DFS: Couldn't find links for " + instr.Assembly);
                    visitedAllLinks = false;
                    continue;
                }
                for (int i = links.Count - 1; i >= 0; i--)
                {
                    var link = links[i];
                    if ((type & link.Type) == 0)
                    {
                        continue;
                    }
                    if (link.Type == LinkType.MemoryJump && link.Base != OperandType.None)
                    {
                        Logger.Warn("DFS: Couldn't find links for " + instr.Assembly);
                        visitedAllLinks = false;
                        continue;
                    }
                    var linkOffset = (reverse ? link.Offset : link.TargetOffset);
                    if (linkOffset >= _firstAddressAfterCode)
                    {
                        Logger.Warn("DFS: Jump outside of code section");
                        visitedAllLinks = false;
                        continue;
                    }
                    if (!visited.Contains(linkOffset))
                    {
                        stack.Push(Tuple.Create(linkOffset, link));
                    }
                }
            }
            return visitedAllLinks;
        }

        private int ToByteArrayIndex(ulong address)
        {
            return _bytes.Offset + (int)(address - _firstAddress);
        }
    }
}
