using NDis86;
using NLog;
using System;
using System.Collections.Generic;
using System.Linq;

namespace UnwindMC.Analysis
{
    public class InstructionGraph
    {
        private readonly Logger Logger = LogManager.GetCurrentClassLogger();

        private class Link
        {
            public Link(ulong offset, ulong targetOffset, LinkType type)
            {
                Offset = offset;
                TargetOffset = targetOffset;
                Type = type;
            }

            public ulong Offset { get; }
            public ulong TargetOffset { get; }
            public LinkType Type { get; }
        }

        [Flags]
        public enum LinkType
        {
            Next = 0x01,
            Branch = 0x02,
            Call = 0x04,
        }

        private readonly IReadOnlyList<Instruction> _instructions;
        private readonly Dictionary<ulong, Instruction> _instructionsMap;
        private readonly Dictionary<ulong, List<Link>> _instructionLinks = new Dictionary<ulong, List<Link>>();
        private readonly Dictionary<ulong, List<Link>> _reverseLinks = new Dictionary<ulong, List<Link>>();

        public InstructionGraph(IReadOnlyList<Instruction> instructions)
        {
            _instructions = instructions;
            _instructionsMap = instructions.ToDictionary(i => i.Offset);
        }

        public IReadOnlyList<Instruction> Instructions => _instructions;

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

        public bool Contains(ulong offset)
        {
            return _instructionsMap.ContainsKey(offset);
        }

        public bool DFS(ulong offset, LinkType type, Func<Instruction, bool> process)
        {
            var visited = new HashSet<ulong>();
            var stack = new Stack<ulong>();
            stack.Push(offset);
            var visitedAllLinks = true;
            while (stack.Count > 0)
            {
                var current = stack.Pop();
                var instr = _instructionsMap[current];
                visited.Add(current);
                if (!process(instr))
                {
                    continue;
                }

                if (!_instructionLinks.TryGetValue(current, out var links))
                {
                    Logger.Warn("DFS: Couldn't find links for " + _instructionsMap[current].Assembly);
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
                    if (!visited.Contains(link.TargetOffset))
                    {
                        stack.Push(link.TargetOffset);
                    }
                }
            }
            return visitedAllLinks;
        }
    }
}
