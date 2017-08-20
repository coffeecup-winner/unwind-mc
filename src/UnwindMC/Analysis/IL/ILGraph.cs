using System.Collections.Generic;
using UnwindMC.Collections;

namespace UnwindMC.Analysis.IL
{
    public class ILGraph : IGraph<ILInstruction>
    {
        private readonly ISet<ILInstruction> _subgraph;

        private ILGraph(ISet<ILInstruction> subgraph)
        {
            _subgraph = subgraph;
        }

        public ILGraph() { }

        public bool Contains(ILInstruction vertex)
        {
            return _subgraph == null || _subgraph.Contains(vertex);
        }

        public IGraph<ILInstruction> GetSubgraph(ISet<ILInstruction> subgraph)
        {
            return new ILGraph(subgraph);
        }

        public IEnumerable<ILInstruction> GetNeighbors(ILInstruction vertex)
        {
            if (vertex.ConditionalChild != null && vertex.ConditionalChild.Order > vertex.Order && Contains(vertex.ConditionalChild))
            {
                yield return vertex.ConditionalChild;
            }
            if (vertex.DefaultChild != null && vertex.DefaultChild.Order > vertex.Order && Contains(vertex.DefaultChild))
            {
                yield return vertex.DefaultChild;
            }
        }
    }
}
