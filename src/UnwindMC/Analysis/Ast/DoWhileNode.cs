namespace UnwindMC.Analysis.Ast
{
    public class DoWhileNode : IStatementNode
    {
        private ScopeNode _body;
        private IExpressionNode _condition;

        public DoWhileNode(ScopeNode body, IExpressionNode condition)
        {
            _body = body;
            _condition = condition;
        }

        public ScopeNode Body => _body;
        public IExpressionNode Condition => _condition;

        public void Accept(INodeVisitor visitor)
        {
            visitor.Visit(this);
            _body.Accept(visitor);
            _condition.Accept(visitor);
        }
    }
}
