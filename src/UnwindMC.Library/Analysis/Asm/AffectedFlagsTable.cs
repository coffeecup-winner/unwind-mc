using System;
using System.Linq;
using NDis86;

namespace UnwindMC.Analysis.Asm
{
    public static class AffectedFlagsTable
    {
        private static readonly FlagsRegister[] Table;

        public static readonly FlagsRegister[] AllFlags = Enum.GetValues(typeof(FlagsRegister)).Cast<FlagsRegister>().ToArray();

        static AffectedFlagsTable()
        {
            Table = new FlagsRegister[Enum.GetValues(typeof(MnemonicCode)).Length];

            const FlagsRegister none = 0;
            const FlagsRegister all = FlagsRegister.Overflow | FlagsRegister.Sign | FlagsRegister.Zero | FlagsRegister.Adjust | FlagsRegister.Carry | FlagsRegister.Parity;

            Table[(int)MnemonicCode.Iadd] = all;
            Table[(int)MnemonicCode.Iand] = all;
            Table[(int)MnemonicCode.Icall] = none;
            Table[(int)MnemonicCode.Icdq] = none;
            Table[(int)MnemonicCode.Icmovl] = none;
            Table[(int)MnemonicCode.Icmp] = all;
            Table[(int)MnemonicCode.Idec] = all & ~FlagsRegister.Carry;
            Table[(int)MnemonicCode.Iidiv] = all;
            Table[(int)MnemonicCode.Iimul] = all;
            Table[(int)MnemonicCode.Ijae] = none;
            Table[(int)MnemonicCode.Ijmp] = none;
            Table[(int)MnemonicCode.Ijz] = none;
            Table[(int)MnemonicCode.Ijnz] = none;
            Table[(int)MnemonicCode.Imov] = none;
            Table[(int)MnemonicCode.Ineg] = all;
            Table[(int)MnemonicCode.Inot] = none;
            Table[(int)MnemonicCode.Ior] = all;
            Table[(int)MnemonicCode.Ipush] = none;
            Table[(int)MnemonicCode.Ipop] = none;
            Table[(int)MnemonicCode.Iret] = none;
            Table[(int)MnemonicCode.Isar] = all;
            Table[(int)MnemonicCode.Ishl] = all;
            Table[(int)MnemonicCode.Isub] = all;
            Table[(int)MnemonicCode.Itest] = all;
            Table[(int)MnemonicCode.Ixor] = all;
        }

        public static FlagsRegister GetAffectedFlags(MnemonicCode code)
        {
            return Table[(int)code];
        }
    }
}
