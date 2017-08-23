using UnwindMC.Util;

namespace UnwindMC.Generation.Ast
{
    public class ReturnNode : IStatementNode
    {
        private Option<VarNode> _var;

        public ReturnNode(Option<VarNode> var)
        {
            _var = var;
        }

        public Option<VarNode> Var => _var;

        public void Accept(INodeTransformer transformer)
        {
            var newNode = transformer.Transform(this);
            if (newNode != this)
            {
                _var = newNode._var;
            }
        }
    }
}
