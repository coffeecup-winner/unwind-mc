using System;
using System.Collections.Generic;
using Type = UnwindMC.Analysis.Data.Type;

namespace UnwindMC.Analysis.Ast.Transformations
{
    public class FixupPointerArithmetics : NodeVisitorBase
    {
        IReadOnlyDictionary<string, Type> _variableTypes;

        public FixupPointerArithmetics(IReadOnlyDictionary<string, Type> variableTypes)
        {
            _variableTypes = variableTypes;
        }

        public override void Visit(BinaryOperatorNode node)
        {
            if (node.Operator != Operator.Add && node.Operator != Operator.Subtract)
            {
                return;
            }
            VarNode var = node.Left as VarNode;
            if (var == null)
            {
                var = node.Right as VarNode;
                if (var == null)
                {
                    return;
                }
            }
            ValueNode value = (node.Left == var ? node.Right : node.Left) as ValueNode;
            if (value == null)
            {
                return;
            }

            var type = _variableTypes[var.Name];
            if (type.IndirectionLevel > 0 || type.IsFunction)
            {
                if (value.Value % type.Size != 0)
                {
                    throw new InvalidOperationException("Value size must be divisible by type size");
                }
                value.Value /= type.Size;
            }
        }
    }
}
