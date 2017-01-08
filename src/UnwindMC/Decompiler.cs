using NDis86;
using System;
using UnwindMC.Analysis;

namespace UnwindMC
{
    public class Decompiler
    {
        public void Decompile(PEFile pe)
        {
            var disassembler = new Disassembler(syntax: DisassemblySyntax.Intel, pc: pe.ImageBase + pe.TextOffset);
            var textBytes = pe.GetTextBytes();
            var instructions = disassembler.Disassemble(textBytes.Array, textBytes.Offset, textBytes.Count, withHex: true, withAssembly: true);
            var analyzer = new Analyzer(instructions);
            analyzer.Analyze();
            Console.WriteLine(analyzer.DumpResults());
        }
    }
}
