using System;
using System.Collections.Generic;
using UnwindMC.Collections;
using UnwindMC.Util;

namespace UnwindMC.Analysis.IL
{
    public class ILGraph : IGraph<ILInstruction, ILInstruction, object>
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

        public ILInstruction GetVertex(ILInstruction vertexId)
        {
            return vertexId;
        }

        public IGraph<ILInstruction, ILInstruction, object> GetSubgraph(ISet<ILInstruction> subgraph)
        {
            return new ILGraph(subgraph);
        }

        public IEnumerable<Either<(ILInstruction vertex, object edge), string>> GetAdjacent(ILInstruction vertex, Func<object, bool> filterEdges)
        {
            if (vertex.ConditionalChild != null && vertex.ConditionalChild.Order > vertex.Order && Contains(vertex.ConditionalChild))
            {
                yield return (vertex.ConditionalChild, null);
            }
            if (vertex.DefaultChild != null && vertex.DefaultChild.Order > vertex.Order && Contains(vertex.DefaultChild))
            {
                yield return (vertex.DefaultChild, null);
            }
        }

        public IGraph<ILInstruction, ILInstruction, object> ReverseEdges()
        {
            throw new NotSupportedException();
        }
    }
}
