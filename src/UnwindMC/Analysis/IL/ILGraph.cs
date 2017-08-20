using System.Collections.Generic;
using UnwindMC.Collections;

namespace UnwindMC.Analysis.IL
{
    public class ILGraph : IGraph<ILInstruction>
    {
        public IEnumerable<ILInstruction> GetNeighbors(ILInstruction vertex)
        {
            if (vertex.ConditionalChild != null && vertex.ConditionalChild.Order > vertex.Order)
            {
                yield return vertex.ConditionalChild;
            }
            if (vertex.DefaultChild != null && vertex.DefaultChild.Order > vertex.Order)
            {
                yield return vertex.DefaultChild;
            }
        }
    }
}
