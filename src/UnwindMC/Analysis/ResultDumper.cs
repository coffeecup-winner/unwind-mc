using NDis86;
using NLog;
using System.Collections.Generic;
using System.Text;
using UnwindMC.Analysis.IL;
using UnwindMC.Util;

namespace UnwindMC.Analysis
{
    public class ResultDumper
    {
        private static readonly Logger Logger = LogManager.GetCurrentClassLogger();

        private readonly InstructionGraph _graph;
        private readonly IDictionary<ulong, Function> _functions;

        public ResultDumper(InstructionGraph graph, IDictionary<ulong, Function> functions)
        {
            _graph = graph;
            _functions = functions;
        }

        public string DumpResults()
        {
            Logger.Info("Dumping results");
            var sb = new StringBuilder();
            int unresolvedInstructions = 0;
            int incompleteInstructions = 0;
            foreach (var instr in _graph.Instructions)
            {
                var address = _graph.GetExtraData(instr.Offset).FunctionAddress;
                string description;
                if (address == 0)
                {
                    if (instr.Code == MnemonicCode.Inop || instr.Code == MnemonicCode.Iint3 || instr.Assembly == "mov edi, edi" || instr.Assembly == "lea ecx, [ecx]")
                    {
                        description = "--------";
                    }
                    else if (instr.Code == MnemonicCode.Inone)
                    {
                        description = "jmptable";
                    }
                    else
                    {
                        description = "????????";
                        unresolvedInstructions++;
                    }
                }
                else if (_functions[address].Status == FunctionStatus.BoundsNotResolvedIncompleteGraph)
                {
                    description = "xxxxxxxx";
                    incompleteInstructions++;
                }
                else
                {
                    description = string.Format("{0:x8}", address);
                }
                sb.AppendFormat("{0} {1:x8} {2,20} {3}", description, instr.Offset, instr.Hex, instr.Assembly);
                var importName = _graph.GetExtraData(instr.Offset).ImportName;
                if (importName != null)
                {
                    sb.Append(" ; " + importName);
                }
                sb.AppendLine();
            }
            var result = sb.ToString();
            Logger.Info("Done: {0} ({1:0%}) unresolved, {2} ({3:0%}) incomplete",
                unresolvedInstructions, (double)unresolvedInstructions / _graph.Instructions.Count,
                incompleteInstructions, (double)incompleteInstructions / _graph.Instructions.Count);
            return result;
        }

        public string DumpFunctionCallGraph()
        {
            Logger.Info("Dumping function call graph");
            var sb = new StringBuilder();
            sb.AppendLine("digraph functions {");
            foreach (var function in _functions.Values)
            {
                sb.AppendLine(string.Format("  sub_{0:x8}", function.Address));
            }
            foreach (var instr in _graph.Instructions)
            {
                if (instr.Code == MnemonicCode.Icall && instr.Operands[0].Type == OperandType.ImmediateBranch)
                {
                    sb.AppendLine(string.Format("  sub_{0:x8} -> sub_{1:x8}", _graph.GetExtraData(instr.Offset).FunctionAddress, instr.GetTargetAddress()));
                }
            }
            sb.AppendLine("}");
            var result = sb.ToString();
            Logger.Info("Done");
            return result;
        }

        public string DumpILGraph(ILInstruction il)
        {
            Logger.Info("Dumping IL graph");
            var sb = new StringBuilder();
            sb.AppendLine("digraph il {");
            var visited = new HashSet<ILInstruction>();
            var queue = new Queue<ILInstruction>();
            queue.Enqueue(il);
            visited.Add(il);
            while (queue.Count > 0)
            {
                var instr = queue.Dequeue();
                sb.AppendLine(string.Format("  {0} [label=\"{1}\"]", instr.GetHashCode(), instr.ToString()));
                if (instr.DefaultChild != null)
                {
                    if (visited.Add(instr.DefaultChild))
                    {
                        queue.Enqueue(instr.DefaultChild);
                    }
                    sb.AppendLine(string.Format("  {0} -> {1} [label=\"{2}\"]", instr.GetHashCode(),
                        instr.DefaultChild.GetHashCode(), instr.ConditionalChild == null ? "" : "false"));
                }
                if (instr.ConditionalChild != null)
                {
                    if (visited.Add(instr.ConditionalChild))
                    {
                        queue.Enqueue(instr.ConditionalChild);
                    }
                    sb.AppendLine(string.Format("  {0} -> {1} [label=\"{2}\"]", instr.GetHashCode(),
                        instr.ConditionalChild.GetHashCode(), "true"));
                }
            }
            sb.AppendLine("}");
            var result = sb.ToString();
            Logger.Info("Done");
            return result;
        }
    }
}
