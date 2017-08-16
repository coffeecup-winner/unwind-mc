namespace UnwindMC.Analysis.Ast
{
    public enum Operator
    {
        Equal,
        NotEqual,
        Less,
        LessOrEqual,
        Greater,
        GreaterOrEqual,

        Or,
        And,

        Negate,
        Add,
        Subtract,
        Multiply,
        Divide,
        Modulo,
    }

    public class BinaryOperatorNode : IExpressionNode
    {
        private Operator _op;
        private IExpressionNode _left;
        private IExpressionNode _right;

        public BinaryOperatorNode(Operator op, IExpressionNode left, IExpressionNode right)
        {
            _op = op;
            _left = left;
            _right = right;
        }

        public Operator Operator => _op;
        public IExpressionNode Left => _left;
        public IExpressionNode Right => _right;

        public void Accept(INodeVisitor visitor)
        {
            visitor.Visit(this);
            _left.Accept(visitor);
            _right.Accept(visitor);
        }
    }
}
