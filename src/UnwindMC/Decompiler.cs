using System;
using UnwindMC.Analysis;

namespace UnwindMC
{
    public class Decompiler
    {
        public void Decompile(PEFile pe)
        {
            var analyzer = new Analyzer(pe.GetTextBytes(), pe.TextSectionAddress);
            analyzer.AddFunction(pe.EntryPointAddress);
            analyzer.Analyze();
            Console.WriteLine(analyzer.DumpResults());
        }
    }
}
