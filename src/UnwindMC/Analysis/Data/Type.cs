namespace UnwindMC.Analysis.Data
{
    public class Type
    {
        public Type(bool isFunction, int indirectionLevel)
        {
            IsFunction = isFunction;
            IndirectionLevel = indirectionLevel;
        }
        public bool IsFunction { get; }
        public int IndirectionLevel { get; }
        public int Size => 4;
    }
}
