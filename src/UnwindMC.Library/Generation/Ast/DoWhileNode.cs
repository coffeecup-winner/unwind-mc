namespace UnwindMC.Generation.Ast
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

        public void Accept(INodeTransformer transformer)
        {
            var newNode = transformer.Transform(this);
            if (newNode != this)
            {
                _body = newNode._body;
                _condition = newNode._condition;
            }
            _body.Accept(transformer);
            _condition.Accept(transformer);
        }
    }
}
