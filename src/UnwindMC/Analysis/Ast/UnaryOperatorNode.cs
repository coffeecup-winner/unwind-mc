namespace UnwindMC.Analysis.Ast
{
    public class UnaryOperatorNode : IExpressionNode
    {
        private Operator _op;
        private IExpressionNode _operand;

        public UnaryOperatorNode(Operator op, IExpressionNode operand)
        {
            _op = op;
            _operand = operand;
        }

        public Operator Operator => _op;
        public IExpressionNode Operand => _operand;

        public void Accept(INodeVisitor visitor)
        {
            visitor.Visit(this);
            _operand.Accept(visitor);
        }
    }
}
