using UnwindMC.Util;

namespace UnwindMC.Analysis.Ast
{
    public class ReturnNode : IStatementNode
    {
        private readonly Option<VarNode> _var;

        public ReturnNode(Option<VarNode> var)
        {
            _var = var;
        }

        public Option<VarNode> Var => _var;

        public void Accept(INodeVisitor visitor)
        {
            visitor.Visit(this);
        }
    }
}
