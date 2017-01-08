using NDis86;
using System.Collections.Generic;
using System.Linq;

namespace UnwindMC.Analysis
{
    public class InstructionGraph
    {
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

        public enum LinkType
        {
            Next,
            Branch,
            Call,
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
    }
}
