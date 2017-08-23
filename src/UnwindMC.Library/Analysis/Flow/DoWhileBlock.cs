using System.Collections.Generic;
using UnwindMC.Analysis.IL;

namespace UnwindMC.Analysis.Flow
{
    public class DoWhileBlock : IBlock
    {
        private readonly List<IBlock> _children;

        public DoWhileBlock(ILInstruction condition, List<IBlock> children)
        {
            Condition = condition;
            _children = children;
        }

        public ILInstruction Condition { get; }
        public IReadOnlyList<IBlock> Children => _children;
    }
}
