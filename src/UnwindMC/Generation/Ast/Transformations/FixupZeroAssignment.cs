namespace UnwindMC.Generation.Ast.Transformations
{
    public class FixupZeroAssignment : NodeTransformerBase
    {
        public override AssignmentNode Transform(AssignmentNode node)
        {
            if (node.Expression is BinaryOperatorNode binary && binary.Operator == Operator.And
                && binary.Left is VarNode var && var.Name == node.Var.Name
                && binary.Right is ValueNode val && val.Value == 0)
            {
                return new AssignmentNode(node.Var, binary.Right);
            }
            return node;
        }
    }
}
