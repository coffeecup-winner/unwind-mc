using System.IO;
using UnwindMC.Analysis;
using UnwindMC.Project;

namespace UnwindMC
{
    public class Decompiler
    {
        private readonly DecompilationProject _project;

        public Decompiler(DecompilationProject project)
        {
            _project = project;
        }

        public void Decompile()
        {
            var pe = PEFile.Load(_project.ExePath);
            var importResolver = new ImportResolver(pe.ImageBase, pe.GetImportAddressTableBytes(), pe.GetImportBytes());
            var analyzer = new Analyzer(pe.GetTextBytes(), pe.TextSectionAddress, importResolver);
            analyzer.AddFunction(pe.EntryPointAddress);
            analyzer.Analyze();
            File.WriteAllText(_project.OutputPath, analyzer.DumpResults());
            File.WriteAllText(Path.Combine(_project.RootPath, "functions.gv"), analyzer.DumpFunctionCallGraph());
        }
    }
}
