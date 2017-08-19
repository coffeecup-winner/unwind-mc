using System;
using System.Collections.Generic;
using Type = UnwindMC.Analysis.Data.Type;

namespace UnwindMC.Analysis.Ast.Transformations
{
    public class FixupPointerArithmetics : NodeTransformerBase
    {
        private readonly IReadOnlyDictionary<string, Type> _variableTypes;

        public FixupPointerArithmetics(IReadOnlyDictionary<string, Type> variableTypes)
        {
            _variableTypes = variableTypes;
        }

        public override BinaryOperatorNode Transform(BinaryOperatorNode node)
        {
            if (node.Operator != Operator.Add && node.Operator != Operator.Subtract)
            {
                return node;
            }
            VarNode var = node.Left as VarNode;
            if (var == null)
            {
                var = node.Right as VarNode;
                if (var == null)
                {
                    return node;
                }
            }
            bool isVarLeft = node.Left == var;
            ValueNode value = (isVarLeft ? node.Right : node.Left) as ValueNode;
            if (value == null)
            {
                return node;
            }

            var type = _variableTypes[var.Name];
            if (type.IndirectionLevel > 0 || type.IsFunction)
            {
                if (value.Value % type.Size != 0)
                {
                    throw new InvalidOperationException("Value size must be divisible by type size");
                }
                var newValue = new ValueNode(value.Value / type.Size);
                return new BinaryOperatorNode(node.Operator, isVarLeft ? (IExpressionNode) var : newValue, isVarLeft ? (IExpressionNode) newValue : var);
            }

            return node;
        }
    }
}
