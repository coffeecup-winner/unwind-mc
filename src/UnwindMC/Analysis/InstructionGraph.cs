using NDis86;
using NLog;
using System;
using System.Collections.Generic;

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
        private readonly ulong _pc;
        private readonly ArraySegment<byte> _bytes;
        private readonly ulong _firstAddress;
        private readonly ulong _firstAddressAfterCode;
        private readonly SortedDictionary<ulong, Instruction> _instructions = new SortedDictionary<ulong, Instruction>();
        private readonly Dictionary<ulong, List<Link>> _instructionLinks = new Dictionary<ulong, List<Link>>();
        private readonly Dictionary<ulong, List<Link>> _reverseLinks = new Dictionary<ulong, List<Link>>();

        public InstructionGraph(Disassembler disassembler, IReadOnlyList<Instruction> instructions, ArraySegment<byte> bytes, ulong pc)
        {
            _disassembler = disassembler;
            _pc = pc;
            _bytes = bytes;
            _firstAddress = instructions[0].Offset;
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

        public bool AddJumpTableEntry(ulong address)
        {
            var data = ReadUInt32(address);
            if (data < _firstAddress || data >= _firstAddressAfterCode)
            {
                return false;
            }
            int size;
            var dataAddresses = new HashSet<ulong>();
            dataAddresses.Add(address);
            Instruction instr;
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
                var instructions = _disassembler.Disassemble(_bytes.Array, _bytes.Offset + (int)(instrAddress - _pc), extraLength, withHex: true, withAssembly: true);
                foreach (var newInstr in instructions)
                {
                    dataAddresses.Add(newInstr.Offset);
                    _instructions[newInstr.Offset] = newInstr;
                }
                size = instr.Length - extraLength;
            }
            var dataInstr = new Instruction(address, MnemonicCode.Inone, 4, null, string.Format("dd {0:x8}", data),
                new Operand[0], 0, OperandType.None, false, false, 0, 0, 0, 0, 0);
            _instructions[address] = dataInstr;
            while (size < 4)
            {
                instr = _instructions[address + (ulong)size];
                _instructions.Remove(address + (ulong)size);
                size += instr.Length;
                dataAddresses.Add(instr.Offset);
            }
            if (size != 4)
            {
                // Re-disassemble the rest; it should be a part of the next table entry or nop/int3
                _disassembler.SetPC(address + 4);
                var instructions = _disassembler.Disassemble(_bytes.Array, _bytes.Offset + (int)(address - _pc) + 4, size - 4, withHex: true, withAssembly: true);
                foreach (var newInstr in instructions)
                {
                    dataAddresses.Add(newInstr.Offset);
                    _instructions[newInstr.Offset] = newInstr;
                }
            }
            return true;
        }

        private uint ReadUInt32(ulong address)
        {
            uint result = 0;
            int baseIndex = _bytes.Offset + (int)(address - _pc);
            for (int i = 3; i >= 0; i--)
            {
                result <<= 8;
                result += _bytes.Array[baseIndex + i];
            }
            return result;
        }

        public bool DFS(ulong offset, LinkType type, Func<Instruction, Link, bool> process)
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
                if (!_instructionLinks.TryGetValue(current.Item1, out links))
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
                    if (link.TargetOffset >= _firstAddressAfterCode)
                    {
                        Logger.Warn("DFS: Jump outside of code section");
                        visitedAllLinks = false;
                        continue;
                    }
                    if (!visited.Contains(link.TargetOffset))
                    {
                        stack.Push(Tuple.Create(link.TargetOffset, link));
                    }
                }
            }
            return visitedAllLinks;
        }
    }
}
