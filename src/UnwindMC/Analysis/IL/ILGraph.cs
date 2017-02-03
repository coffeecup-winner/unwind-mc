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

                if (instr.ConditionalChild != null)
                {
                    if (visited.Add(instr.ConditionalChild) &&
                        (subGraph == null || subGraph.Contains(instr.ConditionalChild)) &&
                        (guard == null || !guard.Contains(instr.ConditionalChild)))
                    {
                        stack.Push(instr.ConditionalChild);
                    }
                }

                if (instr.DefaultChild != null)
                {
                    if (visited.Add(instr.DefaultChild) &&
                        (subGraph == null || subGraph.Contains(instr.DefaultChild)) &&
                        (guard == null || !guard.Contains(instr.DefaultChild)))
                    {
                        stack.Push(instr.DefaultChild);
                    }
                }                
            }
        }
    }
}
