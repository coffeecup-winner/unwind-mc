using System;
using System.Collections.Generic;
using System.Linq;
using UnwindMC.Analysis.IL;
using UnwindMC.Collections;
using UnwindMC.Util;

namespace UnwindMC.Analysis.Flow
{
    public static class FlowAnalyzer
    {
        public static List<IBlock> Analyze(ILInstruction il)
        {
            var doWhileLoops = new Queue<(int, int)>();
            foreach (var loop in FindDoWhileLoops(il))
            {
                doWhileLoops.Enqueue(loop);
            }
            return Analyze(il, null, doWhileLoops);
        }

        private static List<IBlock> Analyze(ILInstruction il, ISet<ILInstruction> subGraph, Queue<(int childOrder, int order)> doWhileLoops, int conditionToIgnore = -1)
        {
            var result = new List<IBlock>();
            var seq = new SequentialBlock();
            var graph = new ILGraph();
            foreach (var instr in graph.BFS(il, subGraph))
            {
                if (doWhileLoops.Count > 0 && doWhileLoops.Peek().childOrder == instr.Order)
                {
                    // the instructions is the beginning of the do-while loop
                    var order = doWhileLoops.Dequeue().order;
                    var body = graph.BFS(instr, subGraph)
                        .Where(i => i.Order <= order)
                        .ToList();
                    var condition = body.Last();
                    body.RemoveAt(body.Count - 1);
                    result.Add(seq);
                    result.Add(new DoWhileBlock(condition, Analyze(instr, body.ToSet(), doWhileLoops, conditionToIgnore: order)));
                    var next = condition.DefaultChild;
                    if (subGraph == null || subGraph.Contains(next))
                    {
                        result.AddRange(Analyze(next, graph.BFS(next, subGraph).ToSet(), doWhileLoops, conditionToIgnore));
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
                var left = graph.BFS(instr.ConditionalChild, subGraph).ToList();
                var right = graph.BFS(instr.DefaultChild, subGraph).ToList();

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

        public static IReadOnlyList<(int, int)> FindDoWhileLoops(ILInstruction il)
        {
            var result = new List<(int childOrder, int order)>();
            foreach (var instr in new ILGraph().BFS(il))
            {
                if (instr.ConditionalChild != null && instr.ConditionalChild.Order < instr.Order)
                {
                    result.Add((instr.ConditionalChild.Order, instr.Order));
                }
            }
            return result
                .OrderBy(c => c.childOrder)
                .ThenByDescending(c => c.order)
                .ToList();
        }
    }
}
