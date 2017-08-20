using System.Collections.Generic;

namespace UnwindMC.Collections
{
    public interface IGraph<TVertex>
    {
        bool Contains(TVertex vertex);
        IGraph<TVertex> GetSubgraph(ISet<TVertex> subgraph);
        IEnumerable<TVertex> GetNeighbors(TVertex vertex);
    }

    public static class GraphExtensions
    {
        public static IEnumerable<TVertex> BFS<TVertex>(this IGraph<TVertex> graph, TVertex start)
        {
            if (!graph.Contains(start))
            {
                yield break;
            }
            var queue = new Queue<TVertex>();
            queue.Enqueue(start);
            var visited = new HashSet<TVertex> { start };
            while (queue.Count > 0)
            {
                var instr = queue.Dequeue();
                yield return instr;

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
