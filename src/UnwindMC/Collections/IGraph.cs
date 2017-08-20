using System.Collections.Generic;

namespace UnwindMC.Collections
{
    public interface IGraph<TVertex>
    {
        IEnumerable<TVertex> GetNeighbors(TVertex vertex);
    }

    public static class GraphExtensions
    {
        public static IEnumerable<TVertex> BFS<TVertex>(this IGraph<TVertex> instruction, TVertex start, ISet<TVertex> subGraph = null)
        {
            if (subGraph != null && !subGraph.Contains(start))
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

                foreach (var neighbor in instruction.GetNeighbors(instr))
                {
                    if (visited.Add(neighbor) && (subGraph == null || subGraph.Contains(neighbor)))
                    {
                        queue.Enqueue(neighbor);
                    }
                }
            }
        }
    }
}
