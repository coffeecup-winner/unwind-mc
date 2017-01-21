namespace UnwindMC.Analysis
{
    public interface IImportResolver
    {
        bool IsImportAddress(ulong address);
        string GetImportName(ulong address);
    }
}
