namespace UnwindMC.Analysis.Ast
{
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

        public void Accept(INodeTransformer transformer)
        {
            var newNode = transformer.Transform(this);
            if (newNode != this)
            {
                _op = newNode._op;
                _left = newNode._left;
                _right = newNode._right;
            }
            _left.Accept(transformer);
            _right.Accept(transformer);
        }
    }
}
