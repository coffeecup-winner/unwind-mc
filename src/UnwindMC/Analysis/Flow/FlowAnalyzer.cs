using System.Collections.Generic;
using System.Linq;
using UnwindMC.Analysis.IL;
using UnwindMC.Util;

namespace UnwindMC.Analysis.Flow
{
    public static class FlowAnalyzer
    {
        public static List<IBlock> Analyze(ILInstruction il, ISet<ILInstruction> subGraph = null)
        {
            var result = new List<IBlock>();
            SequentialBlock seq = new SequentialBlock();
            foreach (var instr in il.DFS(subGraph))
            {
                if (instr.ConditionalChild == null)
                {
                    seq.Add(instr);
                    continue;
                }
                result.Add(seq);
                
                // loop detection
                var guard = new HashSet<ILInstruction> { instr };
                var left = instr.ConditionalChild.DFS(subGraph, guard).ToList();
                var right = instr.DefaultChild.DFS(subGraph, guard).ToList();
                bool leftLoop = left.Any(i => i.DefaultChild == instr || i.ConditionalChild == instr);
                if (leftLoop)
                {
                    result.Add(new LoopBlock(instr, Analyze(instr.ConditionalChild, left.ToSet())));
                    result.AddRange(Analyze(right[0], right.ToSet()));
                    return result;
                }
                bool rightLoop = right.Any(i => i.DefaultChild == instr || i.ConditionalChild == instr);
                if (rightLoop)
                {
                    result.Add(new LoopBlock(instr, Analyze(instr.DefaultChild, right.ToSet())));
                    result.AddRange(Analyze(left[0], left.ToSet()));
                    return result;
                }

                // if it's not a loop, it's a conditional
                int i0 = left.Count - 1;
                int i1 = right.Count - 1;
                while (i0 >= 0 && i1 >= 0 && left[i0] == right[i1])
                {
                    i0--;
                    i1--;
                }
                i0++;
                i1++;
                result.Add(new ConditionalBlock(
                    instr,
                    Analyze(left[0], left.Take(i0).ToSet()),
                    Analyze(right[0], right.Take(i1).ToSet())));
                result.AddRange(Analyze(left[i0], left.Skip(i0).ToSet()));
                return result;
            }
            result.Add(seq);
            return result;
        }
    }
}
