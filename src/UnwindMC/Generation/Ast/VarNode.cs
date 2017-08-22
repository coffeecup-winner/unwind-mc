using UnwindMC.Util;

namespace UnwindMC.Generation.Ast
{
    public class VarNode : IExpressionNode
    {
        private string _name;

        public VarNode(string name)
        {
            _name = name;
        }

        public string Name => _name;

        public void Accept(INodeTransformer transformer)
        {
            var newNode = transformer.Transform(this);
            if (newNode != this)
            {
                _name = newNode._name;
            }
        }
    }

    public static partial class PatternMatching
    {
        private struct VarNodePattern : IPattern<VarNode>
        {
            private readonly IPattern<string> _name;

            public VarNodePattern(IPattern<string> name)
            {
                _name = name;
            }

            public bool Match(VarNode var) => _name.Match(var.Name);

            public bool Match(object var) => var is VarNode obj && Match(obj);
        }

        public static IPattern<VarNode> Var(IPattern<string> name)
        {
            return new VarNodePattern(name);
        }
    }
}
