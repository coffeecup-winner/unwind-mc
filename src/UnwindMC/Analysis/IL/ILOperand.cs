using NDis86;
using System.Text;

namespace UnwindMC.Analysis.IL
{
    public class ILOperand
    {
        private ILOperand(ILOperandType type, OperandType register, int offset, int value)
        {
            Type = type;
            Register = register;
            Offset = offset;
            Value = value;
        }

        public ILOperandType Type { get; }
        public OperandType Register { get; }
        public int Value { get; }
        public int Offset { get; }

        public static ILOperand FromValue(int value)
        {
            return new ILOperand(ILOperandType.Value, OperandType.None, 0, value);
        }

        public static ILOperand FromRegister(OperandType reg)
        {
            return new ILOperand(ILOperandType.Register, reg, 0, 0);
        }

        public static ILOperand FromStack(int offset)
        {
            return new ILOperand(ILOperandType.Stack, OperandType.None, offset, 0);
        }

        public static ILOperand FromPointer(OperandType reg, int offset)
        {
            return new ILOperand(ILOperandType.Pointer, reg, offset, 0);
        }

        public override bool Equals(object obj)
        {
            if (!(obj is ILOperand that))
            {
                return false;
            }
            return Type == that.Type && Register == that.Register && Value == that.Value && Offset == that.Offset;
        }

        public override int GetHashCode()
        {
            int hash = 17;
            hash = hash * 37 + Type.GetHashCode();
            hash = hash * 37 + Register.GetHashCode();
            hash = hash * 37 + Value.GetHashCode();
            hash = hash * 37 + Offset.GetHashCode();
            return hash;
        }

        public override string ToString()
        {
            var sb = new StringBuilder();
            sb.Append(Type);
            sb.Append(": ");
            switch (Type)
            {
                case ILOperandType.Value:
                    sb.Append(Value);
                    break;
                case ILOperandType.Register:
                    sb.Append(Register);
                    break;
                case ILOperandType.Stack:
                    sb.Append(Offset);
                    break;
                case ILOperandType.Pointer:
                    sb.Append("*(");
                    sb.Append(Register);
                    sb.Append(" + ");
                    sb.Append(Offset);
                    sb.Append(")");
                    break;
            }
            return sb.ToString();
        }
    }

    public enum ILOperandType
    {
        Value,
        Register,
        Stack,
        Pointer,
    }
}
