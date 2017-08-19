using System;

namespace UnwindMC.Analysis.Asm
{
    [Flags]
    public enum FlagsRegister : ushort
    {
        Carry = 1 << 0,
        Parity = 1 << 2,
        Adjust = 1 << 4,
        Zero = 1 << 6,
        Sign = 1 << 7,
        Trap = 1 << 8,
        InterruptEnable = 1 << 9,
        Direction = 1 << 10,
        Overflow = 1 << 11,
    }
}
