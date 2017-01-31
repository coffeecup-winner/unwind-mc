using System.Collections.Generic;

namespace UnwindMC.Analysis.IL
{
    public static class ILGraph
    {
        public static IEnumerable<ILInstruction> DFS(this ILInstruction instruction, ISet<ILInstruction> subGraph = null, ISet<ILInstruction> guard = null)
        {
            if ((subGraph != null && !subGraph.Contains(instruction)) || (guard != null && guard.Contains(instruction)))
            {
                yield break;
            }
            var stack = new Stack<ILInstruction>();
            stack.Push(instruction);
            var visited = new HashSet<ILInstruction> { instruction };
            while (stack.Count > 0)
            {
                var instr = stack.Pop();
                yield return instr;

                // children will be visited in the reverse order, but it shouldn't matter for most use cases
                foreach (var child in instr.Children)
                {
                    if (visited.Add(child.Value) && (subGraph == null || subGraph.Contains(child.Value)) && (guard == null || !guard.Contains(child.Value)))
                    {
                        stack.Push(child.Value);
                    }
                }
            }
        }
    }
}
