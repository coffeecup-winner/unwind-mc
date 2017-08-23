namespace UnwindMC.Generation.Ast
{
    public class ValueNode : IExpressionNode
    {
        private int _value;

        public ValueNode(int value)
        {
            _value = value;
        }

        public int Value => _value;

        public void Accept(INodeTransformer transformer)
        {
            var newNode = transformer.Transform(this);
            if (newNode != this)
            {
                _value = newNode._value;
            }
        }
    }
}
