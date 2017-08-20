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
        IGraph<TVertexId, TVertex, TEdge> GetSubgraph(ISet<TVertex> subgraph);
        IEnumerable<TVertexId> GetNeighbors(TVertexId vertex);
        IEnumerable<Either<(TVertexId vertex, TEdge edge), string>> GetAdjacent(TVertexId vertex, Func<TEdge, bool> filterEdges);
        IGraph<TVertexId, TVertex, TEdge> ReverseEdges();
    }

    public static class GraphExtensions
    {
        private static readonly Logger Logger = LogManager.GetCurrentClassLogger();

        public static bool DFS<TVertexId, TVertex, TEdge>(this IGraph<TVertexId, TVertex, TEdge> graph, TVertexId start, Func<TEdge, bool> filterEdges, Func<TVertex, TEdge, bool> process)
        {
            var visited = new HashSet<TVertexId>();
            var stack = new Stack<(TVertexId vertexId, TEdge edge)>();
            stack.Push((start, default(TEdge)));
            visited.Add(start);
            var visitedAllLinks = true;
            while (stack.Count > 0)
            {
                var current = stack.Pop();
                if (!process(graph.GetVertex(current.vertexId), current.edge))
                {
                    continue;
                }

                foreach (var adj in graph.GetAdjacent(current.vertexId, filterEdges).Reverse())
                {
                    if (adj.IsRight)
                    {
                        Logger.Warn(adj.Right);
                        visitedAllLinks = false;
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
            return visitedAllLinks;
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
                var instr = queue.Dequeue();
                yield return graph.GetVertex(instr);

                foreach (var neighbor in graph.GetNeighbors(instr))
                {
                    if (visited.Add(neighbor))
                    {
                        queue.Enqueue(neighbor);
                    }
                }
            }
        }
    }
}
