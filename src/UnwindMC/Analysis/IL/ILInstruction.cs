using System;
using System.Collections.Generic;
using System.Text;

namespace UnwindMC.Analysis.IL
{
    public class ILInstruction
    {
        private readonly Dictionary<ILBranchType, ILInstruction> _children = new Dictionary<ILBranchType, ILInstruction>();

        public ILInstruction(ILInstructionType type, ILOperand target = null, ILOperand source = null, ILBranch branch = null)
        {
            Type = type;
            Target = target;
            Source = source;
            Branch = branch;
        }

        public ILInstructionType Type { get; }
        public ILOperand Target { get; }
        public ILOperand Source { get; }
        public ILBranch Branch { get; }
        public IReadOnlyDictionary<ILBranchType, ILInstruction> Children => _children;

        public void AddNext(ILInstruction instr)
        {
            _children.Add(ILBranchType.Next, instr);
        }

        public void AddLeft(ILBranchType type, ILInstruction instr)
        {
            _children.Add(type, instr);
        }

        public void AddRight(ILBranchType type, ILInstruction instr)
        {
            _children.Add(Complement(type), instr);
        }

        private static ILBranchType Complement(ILBranchType type)
        {
            switch (type)
            {
                case ILBranchType.Equal: return ILBranchType.NotEqual;
                case ILBranchType.NotEqual: return ILBranchType.Equal;
                case ILBranchType.Less: return ILBranchType.GreaterOrEqual;
                case ILBranchType.LessOrEqual: return ILBranchType.Greater;
                case ILBranchType.GreaterOrEqual: return ILBranchType.Less;
                case ILBranchType.Greater: return ILBranchType.LessOrEqual;
                default: throw new ArgumentException("Cannot find branch type complement");
            }
        }

        public override string ToString()
        {
            var sb = new StringBuilder();
            switch (Type)
            {
                case ILInstructionType.None:
                    sb.Append("<none>");
                    break;
                case ILInstructionType.Virtual:
                    sb.Append(Branch);
                    break;
                case ILInstructionType.Add:
                    sb.Append(Target);
                    sb.Append(" += ");
                    sb.Append(Source);
                    break;
                case ILInstructionType.Assign:
                    sb.Append(Target);
                    sb.Append(" = ");
                    sb.Append(Source);
                    break;
                case ILInstructionType.Call:
                    sb.Append("Call ");
                    sb.Append(Target);
                    break;
                case ILInstructionType.Compare:
                    sb.Append(Target);
                    sb.Append(" <> ");
                    sb.Append(Source);
                    break;
                case ILInstructionType.Return:
                    sb.Append("return");
                    break;
            }
            return sb.ToString();
        }
    }

    public enum ILInstructionType
    {
        None,
        Virtual,

        Add,
        Assign,
        Call,
        Compare,
        Return,
    }
}
