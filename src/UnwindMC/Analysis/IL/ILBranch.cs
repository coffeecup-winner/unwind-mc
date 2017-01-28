namespace UnwindMC.Analysis.IL
{
    public class ILBranch
    {
        public ILBranch(ILBranchType type, ulong address)
        {
            Type = type;
            Address = address;
        }

        public ILBranchType Type { get; }
        public ulong Address { get; }

        public override bool Equals(object obj)
        {
            var that = obj as ILBranch;
            if (that == null)
            {
                return false;
            }
            return Type == that.Type && Address == that.Address;
        }

        public override int GetHashCode()
        {
            int hash = 17;
            hash = hash * 37 + Type.GetHashCode();
            hash = hash * 37 + Address.GetHashCode();
            return hash;
        }
    }

    public enum ILBranchType
    {
        Equal,
        NotEqual,
        Less,
        LessOrEqual,
        GreaterOrEqual,
        Greater,
        Next,
    }
}
