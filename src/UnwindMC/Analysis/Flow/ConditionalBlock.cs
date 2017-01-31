using System.Collections.Generic;

namespace UnwindMC.Analysis.Flow
{
    public class ConditionalBlock : IBlock
    {
        private readonly List<IBlock> _left;
        private readonly List<IBlock> _right;

        public ConditionalBlock(List<IBlock> left, List<IBlock> right)
        {
            _left = left;
            _right = right;
        }

        public IReadOnlyList<IBlock> LeftChildren => _left;
        public IReadOnlyList<IBlock> RightChildren => _right;
    }
}
