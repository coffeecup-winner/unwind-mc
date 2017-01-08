using NDis86;
using System.Text;
using UnwindMC.Analysis;

namespace UnwindMC
{
    public class Decompiler
    {
        public InstructionGraph Decompile(PEFile pe)
        {
            var disassembler = new Disassembler(syntax: DisassemblySyntax.Intel, pc: pe.ImageBase + pe.TextOffset);
            var textBytes = pe.GetTextBytes();
            var instructions = disassembler.Disassemble(textBytes.Array, textBytes.Offset, textBytes.Count, withHex: true, withAssembly: true);
            var analyzer = new Analyzer(instructions);
            analyzer.Analyze();
            return analyzer.Graph;
        }

        public string Dump(InstructionGraph graph)
        {
            var sb = new StringBuilder();
            foreach (var instr in graph.Instructions)
            {
                sb.AppendFormat("{0:x8} {1,20} {2}", instr.Offset, instr.Hex, instr.Assembly);
                sb.AppendLine();
            }
            return sb.ToString();
        }
    }
}
