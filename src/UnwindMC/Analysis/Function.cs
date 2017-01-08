namespace UnwindMC.Analysis
{
    public class Function
    {
        public Function(ulong address)
        {
            Address = address;
            Status = FunctionStatus.Created;
        }

        public ulong Address { get; }
        public FunctionStatus Status { get; set; }
    }

    public enum FunctionStatus
    {
        Created,
        BoundsResolved,
        BoundsNotResolvedInvalidAddress,
        BoundsNotResolvedIncompleteGraph,
    }
}
