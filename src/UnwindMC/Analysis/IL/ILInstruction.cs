using System;
using System.Text;

namespace UnwindMC.Analysis.IL
{
    public class ILInstruction
    {
        public ILInstruction(ILInstructionType type, ILOperand target = null, ILOperand source = null, ILBranch branch = null)
        {
            Type = type;
            Target = target;
            TargetId = -1;
            Source = source;
            SourceId = -1;
            Branch = branch;
        }

        public ILInstructionType Type { get; }
        public ILOperand Target { get; }
        public int TargetId { get; private set; }
        public ILOperand Source { get; }
        public int SourceId { get; private set; }
        public ILBranch Branch { get; }
        public ILInstruction DefaultChild { get; private set; }
        public ILBranchType Condition { get; private set; }
        public ILInstruction ConditionalChild { get; private set; }
        public int Order { get; private set; }

        public void AddDefaultChild(ILInstruction instr)
        {
            DefaultChild = instr;
        }

        public void AddConditionalChild(ILBranchType condition, ILInstruction instr)
        {
            Condition = condition;
            ConditionalChild = instr;
        }

        public void SetVariableIds(int targetId, int sourceId)
        {
            TargetId = targetId;
            SourceId = sourceId;
        }

        public void SetOrder(int order)
        {
            Order = order;
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
                    switch (Condition)
                    {
                        case ILBranchType.Equal: sb.Append(" == "); break;
                        case ILBranchType.NotEqual: sb.Append(" != "); break;
                        case ILBranchType.Less: sb.Append(" < "); break;
                        case ILBranchType.LessOrEqual: sb.Append(" <= "); break;
                        case ILBranchType.GreaterOrEqual: sb.Append(" >= "); break;
                        case ILBranchType.Greater: sb.Append(" > "); break;
                        default: throw new InvalidOperationException("Invalid condition type");
                    }
                    sb.Append(Source);
                    break;
                case ILInstructionType.Return:
                    sb.Append("return");
                    break;
                case ILInstructionType.Subtract:
                    sb.Append(Target);
                    sb.Append(" -= ");
                    sb.Append(Source);
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
        Divide,
        Multiply,
        Negate,
        Return,
        Subtract,
    }
}
