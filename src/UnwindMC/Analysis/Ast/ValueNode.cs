namespace UnwindMC.Analysis.Ast
{
    public class ValueNode : IExpressionNode
    {
        public ValueNode(int value)
        {
            Value = value;
        }

        public int Value { get; set; }

        public void Accept(INodeVisitor visitor)
        {
            visitor.Visit(this);
        }
    }
}
