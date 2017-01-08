namespace UnwindMC.Analysis
{
    public class JumpTable
    {
        public JumpTable(ulong address)
        {
            Address = address;
        }

        public ulong Address { get; }
        public int FirstIndex { get; set; }
        public int MaxIndex { get; set; }
    }
}