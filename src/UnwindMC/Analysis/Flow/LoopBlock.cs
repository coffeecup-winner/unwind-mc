using System.Collections.Generic;

namespace UnwindMC.Analysis.Flow
{
    public class LoopBlock : IBlock
    {
        private readonly List<IBlock> _children;

        public LoopBlock(List<IBlock> children)
        {
            _children = children;
        }

        public IReadOnlyList<IBlock> Children => _children;
    }
}
