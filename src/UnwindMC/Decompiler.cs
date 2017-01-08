using System;
using UnwindMC.Analysis;

namespace UnwindMC
{
    public class Decompiler
    {
        public void Decompile(PEFile pe)
        {
            var analyzer = new Analyzer(pe.GetTextBytes(), pe.ImageBase + pe.TextOffset);
            analyzer.Analyze();
            Console.WriteLine(analyzer.DumpResults());
        }
    }
}
