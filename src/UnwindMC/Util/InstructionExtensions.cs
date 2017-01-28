using NDis86;
using System;

namespace UnwindMC.Util
{
    public static class InstructionExtensions
    {
        public static ulong GetTargetAddress(this Instruction instr)
        {
            var targetOffset = instr.Offset + instr.Length;
            return (ulong)((long)targetOffset + instr.Operands[0].GetValue());
        }

        public static long GetValue(this Operand operand)
        {
            switch (operand.Size)
            {
                case 8: return operand.LValue.@sbyte;
                case 16: return operand.LValue.sword;
                case 32: return operand.LValue.sdword;
                case 64: return operand.LValue.sqword;
                default: throw new InvalidOperationException();
            }
        }

        public static long GetMemoryOffset(this Operand operand)
        {
            switch (operand.Offset)
            {
                case 0: return 0;
                case 8: return operand.LValue.@sbyte;
                case 16: return operand.LValue.sword;
                case 32: return operand.LValue.sdword;
                case 64: return operand.LValue.sqword;
                default: throw new InvalidOperationException();
            }
        }
    }
}
