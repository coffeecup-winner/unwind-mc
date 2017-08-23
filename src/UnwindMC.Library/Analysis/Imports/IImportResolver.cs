namespace UnwindMC.Analysis.Imports
{
    public interface IImportResolver
    {
        bool IsImportAddress(ulong address);
        string GetImportName(ulong address);
    }
}
