using System;
using UnwindMC.Analysis;

namespace UnwindMC
{
    public class Decompiler
    {
        public void Decompile(PEFile pe)
        {
            var importResolver = new ImportResolver(pe.ImageBase, pe.GetImportAddressTableBytes(), pe.GetImportBytes());
            var analyzer = new Analyzer(pe.GetTextBytes(), pe.TextSectionAddress, importResolver);
            analyzer.AddFunction(pe.EntryPointAddress);
            analyzer.Analyze();
            Console.WriteLine(analyzer.DumpResults());
        }
    }
}
