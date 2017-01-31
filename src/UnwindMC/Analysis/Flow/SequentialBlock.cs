using System.Collections.Generic;
using UnwindMC.Analysis.IL;

namespace UnwindMC.Analysis.Flow
{
    public class SequentialBlock : IBlock
    {
        private readonly List<ILInstruction> _instructions = new List<ILInstruction>();

        public IReadOnlyList<ILInstruction> Instructions => _instructions;

        public void Add(ILInstruction instr)
        {
            _instructions.Add(instr);
        }
    }
}
