﻿namespace UnwindMC.Analysis.Asm
{
    public class JumpTable
    {
        public JumpTable(ulong reference, ulong address)
        {
            Reference = reference;
            Address = address;
        }

        public ulong Reference { get; }
        public ulong Address { get; }
        public int FirstIndex { get; set; }
        public int Count { get; set; }
    }
}