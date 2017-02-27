using System;
using System.Collections.Generic;
using System.Linq;
using UnwindMC.Analysis.IL;
using UnwindMC.Util;

namespace UnwindMC.Analysis.Flow
{
    public static class FlowAnalyzer
    {
        public static List<IBlock> Analyze(ILInstruction il)
        {
            var doWhileLoops = new Queue<Tuple<int, int>>();
            foreach (var loop in FindDoWhileLoops(il))
            {
                doWhileLoops.Enqueue(loop);
            }
            return Analyze(il, null, doWhileLoops);
        }

        private static List<IBlock> Analyze(ILInstruction il, ISet<ILInstruction> subGraph, Queue<Tuple<int, int>> doWhileLoops, int conditionToIgnore = -1)
        {
            var result = new List<IBlock>();
            SequentialBlock seq = new SequentialBlock();
            foreach (var instr in il.BFS(subGraph))
            {
                if (doWhileLoops.Count > 0 && doWhileLoops.Peek().Item1 == instr.Order)
                {
                    // the instructions is the beginning of the do-while loop
                    var doWhile = doWhileLoops.Dequeue();
                    var body = instr.BFS(subGraph)
                        .Where(i => i.Order <= doWhile.Item2)
                        .ToList();
                    var condition = body.Last();
                    body.RemoveAt(body.Count - 1);
                    result.Add(seq);
                    result.Add(new DoWhileBlock(condition, Analyze(instr, body.ToSet(), doWhileLoops, conditionToIgnore: doWhile.Item2)));
                    var next = condition.DefaultChild;
                    if (subGraph.Contains(next))
                    {
                        result.AddRange(Analyze(next, next.BFS(subGraph).ToSet(), doWhileLoops, conditionToIgnore));
                    }
                    return result;
                }

                if (instr.ConditionalChild == null || instr.Order == conditionToIgnore)
                {
                    seq.Add(instr);
                    continue;
                }
                result.Add(seq);
                
                // loop detection
                var left = instr.ConditionalChild.BFS(subGraph).ToList();
                var right = instr.DefaultChild.BFS(subGraph).ToList();

                bool isConditional = left[left.Count - 1] == right[right.Count - 1];
                if (isConditional)
                {
                    int i0 = left.Count - 2;
                    int i1 = right.Count - 2;
                    while (i0 >= 0 && i1 >= 0 && left[i0] == right[i1])
                    {
                        i0--;
                        i1--;
                    }
                    i0++;
                    i1++;
                    result.Add(new ConditionalBlock(
                        instr,
                        Analyze(left[0], left.Take(i0).ToSet(), doWhileLoops, conditionToIgnore),
                        Analyze(right[0], right.Take(i1).ToSet(), doWhileLoops, conditionToIgnore)));
                    result.AddRange(Analyze(left[i0], left.Skip(i0).ToSet(), doWhileLoops, conditionToIgnore));
                    return result;
                }

                bool leftLoop = left.Any(i => i.DefaultChild == instr || i.ConditionalChild == instr);
                if (leftLoop)
                {
                    result.Add(new WhileBlock(instr, Analyze(instr.ConditionalChild, left.ToSet(), doWhileLoops, conditionToIgnore)));
                    result.AddRange(Analyze(right[0], right.ToSet(), doWhileLoops, conditionToIgnore));
                    return result;
                }

                bool rightLoop = right.Any(i => i.DefaultChild == instr || i.ConditionalChild == instr);
                if (rightLoop)
                {
                    result.Add(new WhileBlock(instr, Analyze(instr.DefaultChild, right.ToSet(), doWhileLoops, conditionToIgnore)));
                    result.AddRange(Analyze(left[0], left.ToSet(), doWhileLoops, conditionToIgnore));
                    return result;
                }

                throw new InvalidOperationException();
            }
            result.Add(seq);
            return result;
        }

        public static IReadOnlyList<Tuple<int, int>> FindDoWhileLoops(ILInstruction il)
        {
            var result = new List<Tuple<int, int>>();
            foreach (var instr in il.BFS())
            {
                if (instr.ConditionalChild != null && instr.ConditionalChild.Order < instr.Order)
                {
                    result.Add(Tuple.Create(instr.ConditionalChild.Order, instr.Order));
                }
            }
            return result
                .OrderBy(c => c.Item1)
                .ThenByDescending(c => c.Item2)
                .ToList();
        }

        private static IEnumerable<ILInstruction> BFS(this ILInstruction instruction, ISet<ILInstruction> subGraph = null)
        {
            // this BFS does not follow up-links
            if ((subGraph != null && !subGraph.Contains(instruction)))
            {
                yield break;
            }
            var queue = new Queue<ILInstruction>();
            queue.Enqueue(instruction);
            var visited = new HashSet<ILInstruction> { instruction };
            while (queue.Count > 0)
            {
                var instr = queue.Dequeue();
                yield return instr;

                if (instr.ConditionalChild != null && instr.ConditionalChild.Order > instr.Order)
                {
                    if (visited.Add(instr.ConditionalChild) &&
                        (subGraph == null || subGraph.Contains(instr.ConditionalChild)))
                    {
                        queue.Enqueue(instr.ConditionalChild);
                    }
                }

                if (instr.DefaultChild != null && instr.DefaultChild.Order > instr.Order)
                {
                    if (visited.Add(instr.DefaultChild) &&
                        (subGraph == null || subGraph.Contains(instr.DefaultChild)))
                    {
                        queue.Enqueue(instr.DefaultChild);
                    }
                }
            }
        }
    }
}
