using UnwindMC.Util;

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

    public static partial class PatternMatching
    {
        private struct ValueNodePattern : IPattern<ValueNode>
        {
            private readonly IPattern _value;

            public ValueNodePattern(IPattern value)
            {
                _value = value;
            }

            public bool Match(ValueNode var) => _value.Match(var.Value);

            public bool Match(object var) => var is ValueNode obj && Match(obj);
        }

        public static IPattern Value(IPattern value)
        {
            return new ValueNodePattern(value);
        }
    }
}
