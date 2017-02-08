namespace UnwindMC.Analysis.Ast
{
    public class ValueNode : IExpressionNode
    {
        private int _value;

        public ValueNode(int value)
        {
            _value = value;
        }

        public int Value => _value;
    }
}
