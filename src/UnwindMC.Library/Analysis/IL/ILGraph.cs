using System;
using System.Collections.Generic;
using UnwindMC.Collections;
using UnwindMC.Util;

namespace UnwindMC.Analysis.IL
{
    public class ILGraph : IGraph<ILInstruction, ILInstruction, object>
    {
        private readonly ISet<ILInstruction> _subgraph;
        private readonly Func<object, bool> _edgePredicate;

        private ILGraph(ISet<ILInstruction> subgraph, Func<object, bool> edgePredicate)
        {
            _subgraph = subgraph;
            _edgePredicate = edgePredicate;
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

        public IEnumerable<Either<(ILInstruction vertex, object edge), string>> GetAdjacent(ILInstruction vertex)
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

        public IGraph<ILInstruction, ILInstruction, object> GetSubgraph(ISet<ILInstruction> subgraph)
        {
            return new ILGraph(subgraph, _edgePredicate);
        }

        public IGraph<ILInstruction, ILInstruction, object> WithEdgeFilter(Func<object, bool> predicate)
        {
            return new ILGraph(_subgraph, predicate);
        }

        public IGraph<ILInstruction, ILInstruction, object> ReverseEdges()
        {
            throw new NotSupportedException();
        }
    }
}
