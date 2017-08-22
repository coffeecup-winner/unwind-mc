using static UnwindMC.Generation.Ast.PatternMatching;
using static UnwindMC.Util.PatternMatching;

namespace UnwindMC.Generation.Ast.Transformations
{
    public class FixupZeroAssignment : NodeTransformerBase
    {
        public override AssignmentNode Transform(AssignmentNode node)
        {
            var v = Capture<ValueNode>();
            return Match(node.Expression,
                Binary(Var(C(node.Var.Name)), C(Operator.And), v % Value(C(0)))
                    .Then(() => new AssignmentNode(node.Var, (ValueNode)v)),
                _.Then(() => node));
        }
    }
}
