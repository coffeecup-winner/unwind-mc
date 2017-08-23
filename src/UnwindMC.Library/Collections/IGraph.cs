using System;
using System.Collections.Generic;
using System.Linq;
using NLog;
using UnwindMC.Util;

namespace UnwindMC.Collections
{
    public interface IGraph<TVertexId, TVertex, TEdge>
    {
        bool Contains(TVertexId vertex);
        TVertex GetVertex(TVertexId vertexId);
        IEnumerable<Either<(TVertexId vertex, TEdge edge), string>> GetAdjacent(TVertexId vertex);

        IGraph<TVertexId, TVertex, TEdge> GetSubgraph(ISet<TVertex> subgraph);
        IGraph<TVertexId, TVertex, TEdge> WithEdgeFilter(Func<TEdge, bool> predicate);
        IGraph<TVertexId, TVertex, TEdge> ReverseEdges();
    }

    public static class GraphExtensions
    {
        private static readonly Logger Logger = LogManager.GetCurrentClassLogger();

        public static bool DFS<TVertexId, TVertex, TEdge>(this IGraph<TVertexId, TVertex, TEdge> graph, TVertexId start, Func<TVertex, TEdge, bool> process)
        {
            var visited = new HashSet<TVertexId>();
            var stack = new Stack<(TVertexId vertexId, TEdge edge)>();
            stack.Push((start, default(TEdge)));
            visited.Add(start);
            var visitedAllEdges = true;
            while (stack.Count > 0)
            {
                var current = stack.Pop();
                if (!process(graph.GetVertex(current.vertexId), current.edge))
                {
                    continue;
                }

                foreach (var adj in graph.GetAdjacent(current.vertexId).Reverse())
                {
                    if (adj.IsRight)
                    {
                        Logger.Warn(adj.Right);
                        visitedAllEdges = false;
                        continue;
                    }
                    var edge = adj.Left;
                    if (!visited.Contains(edge.vertex))
                    {
                        stack.Push(edge);
                        visited.Add(edge.vertex);
                    }
                }
            }
            return visitedAllEdges;
        }

        public static IEnumerable<TVertex> BFS<TVertexId, TVertex, TEdge>(this IGraph<TVertexId, TVertex, TEdge> graph, TVertexId start)
        {
            if (!graph.Contains(start))
            {
                yield break;
            }
            var queue = new Queue<TVertexId>();
            queue.Enqueue(start);
            var visited = new HashSet<TVertexId> { start };
            while (queue.Count > 0)
            {
                var vertexId = queue.Dequeue();
                yield return graph.GetVertex(vertexId);

                foreach (var adj in graph.GetAdjacent(vertexId))
                {
                    if (adj.IsRight)
                    {
                        Logger.Warn(adj.Right);
                        continue;
                    }
                    var edge = adj.Left;
                    if (visited.Add(edge.vertex))
                    {
                        queue.Enqueue(edge.vertex);
                    }
                }
            }
        }
    }
}
