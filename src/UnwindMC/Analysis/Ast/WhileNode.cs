namespace UnwindMC.Analysis.Ast
{
    public class WhileNode : IStatementNode
    {
        private IExpressionNode _condition;
        private ScopeNode _body;

        public WhileNode(IExpressionNode condition, ScopeNode body)
        {
            _condition = condition;
            _body = body;
        }

        public IExpressionNode Condition => _condition;
        public ScopeNode Body => _body;

        public void Accept(INodeTransformer transformer)
        {
            var newNode = transformer.Transform(this);
            if (newNode != this)
            {
                _condition = newNode._condition;
                _body = newNode._body;
            }
            _condition.Accept(transformer);
            _body.Accept(transformer);
        }
    }
}
