using System;
using System.Collections.Generic;
using UnwindMC.Util;
using Type = UnwindMC.Analysis.Data.Type;
using static UnwindMC.Generation.Ast.PatternMatching;
using static UnwindMC.Util.PatternMatching;

namespace UnwindMC.Generation.Ast.Transformations
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
            var var = Capture<VarNode>();
            var value = Capture<ValueNode>();
            return Match(node,
                Binary(var % Var(_), C(Operator.Add), value % Value(_)).Then(() => Fixup(node, var, value)),
                Binary(value % Value(_), C(Operator.Add), var % Var(_)).Then(() => Fixup(node, value, var)),
                Binary(var % Var(_), C(Operator.Subtract), value % Value(_)).Then(() => Fixup(node, var, value)),
                Binary(value % Value(_), C(Operator.Subtract), var % Var(_)).Then(() => Fixup(node, value, var)),
                Otherwise(node));
        }

        private BinaryOperatorNode Fixup(BinaryOperatorNode node, VarNode var, ValueNode value)
        {
            var type = _variableTypes[var.Name];
            if (type.IndirectionLevel > 0 || type.IsFunction)
            {
                if (value.Value % type.Size != 0)
                {
                    throw new InvalidOperationException("Value size must be divisible by type size");
                }
                return new BinaryOperatorNode(node.Operator, var, new ValueNode(value.Value / type.Size));
            }
            return node;
        }

        private BinaryOperatorNode Fixup(BinaryOperatorNode node, ValueNode value, VarNode var)
        {
            return Fixup(node, var, value);
        }
    }
}
