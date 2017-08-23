using System.Collections.Generic;
using UnwindMC.Analysis.IL;

namespace UnwindMC.Analysis.Flow
{
    public class ConditionalBlock : IBlock
    {
        private readonly List<IBlock> _trueBranch;
        private readonly List<IBlock> _falseBranch;

        public ConditionalBlock(ILInstruction condition, List<IBlock> trueBranch, List<IBlock> falseBranch)
        {
            Condition = condition;
            _trueBranch = trueBranch;
            _falseBranch = falseBranch;
        }

        public ILInstruction Condition { get; }
        public IReadOnlyList<IBlock> TrueBranch => _trueBranch;
        public IReadOnlyList<IBlock> FalseBranch => _falseBranch;
    }
}
