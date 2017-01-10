using NDis86;

namespace UnwindMC.Analysis
{
    static class RegisterHelper
    {
        public static OperandType GetLowByteRegisterFromDword(OperandType register)
        {
            switch (register)
            {
                case OperandType.EAX: return OperandType.AL;
                case OperandType.EBX: return OperandType.BL;
                case OperandType.ECX: return OperandType.CL;
                case OperandType.EDX: return OperandType.DL;
                default: return OperandType.None;
            }
        }
    }
}
