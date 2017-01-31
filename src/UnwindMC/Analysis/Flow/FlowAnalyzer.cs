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
                if (instr.Children.Count < 2)
                {
                    seq.Add(instr);
                    continue;
                }
                result.Add(seq);
                
                // loop detection
                var keys = instr.Children.Keys.ToArray();
                var guard = new HashSet<ILInstruction> { instr };
                var left = instr.Children[keys[0]].DFS(subGraph, guard).ToList();
                var right = instr.Children[keys[1]].DFS(subGraph, guard).ToList();
                bool leftLoop = left.Any(i => i.Children.Values.Any(c => c == instr));
                if (leftLoop)
                {
                    result.Add(new LoopBlock(Analyze(instr.Children[keys[0]], left.ToSet())));
                    result.AddRange(Analyze(right[0], right.ToSet()));
                    return result;
                }
                bool rightLoop = right.Any(i => i.Children.Values.Any(c => c == instr));
                if (rightLoop)
                {
                    result.Add(new LoopBlock(Analyze(instr.Children[keys[1]], right.ToSet())));
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
