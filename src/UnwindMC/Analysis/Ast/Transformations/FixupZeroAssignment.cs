namespace UnwindMC.Analysis.Ast.Transformations
{
    public class FixupZeroAssignment : NodeVisitorBase
    {
        public override void Visit(AssignmentNode node)
        {
            if (node.Expression is BinaryOperatorNode binary && binary.Operator == Operator.And
                && binary.Left is VarNode var && var.Name == node.Var.Name
                && binary.Right is ValueNode val && val.Value == 0)
            {
                node.Expression = binary.Right;
            }
            base.Visit(node);
        }
    }
}
