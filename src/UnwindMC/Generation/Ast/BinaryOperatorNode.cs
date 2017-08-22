using UnwindMC.Util;

namespace UnwindMC.Generation.Ast
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

    public static partial class PatternMatching
    {
        private struct BinaryOperatorNodePattern : IPattern<BinaryOperatorNode>
        {
            private readonly IPattern _left;
            private readonly IPattern _op;
            private readonly IPattern _right;

            public BinaryOperatorNodePattern(IPattern left, IPattern op, IPattern right)
            {
                _left = left;
                _op = op;
                _right = right;
            }

            public bool Match(BinaryOperatorNode var) => _left.Match(var.Left) && _op.Match(var.Operator) && _right.Match(var.Right);

            public bool Match(object var) => var is BinaryOperatorNode obj && Match(obj);
        }

        public static IPattern<BinaryOperatorNode> Binary(IPattern left, IPattern op, IPattern right)
        {
            return new BinaryOperatorNodePattern(left, op, right);
        }
    }
}
