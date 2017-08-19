namespace UnwindMC.Analysis.Ast
{
    public class IfThenElseNode : IStatementNode
    {
        private IExpressionNode _condition;
        private ScopeNode _trueBranch;
        private ScopeNode _falseBranch;

        public IfThenElseNode(IExpressionNode condition, ScopeNode trueBranch, ScopeNode falseBranch)
        {
            _condition = condition;
            _trueBranch = trueBranch;
            _falseBranch = falseBranch;
        }

        public IExpressionNode Condition => _condition;
        public ScopeNode TrueBranch => _trueBranch;
        public ScopeNode FalseBranch => _falseBranch;

        public void Accept(INodeTransformer transformer)
        {
            var newNode = transformer.Transform(this);
            if (newNode != this)
            {
                _condition = newNode._condition;
                _trueBranch = newNode._trueBranch;
                _falseBranch = newNode._falseBranch;
            }
            _condition.Accept(transformer);
            _trueBranch.Accept(transformer);
            _falseBranch.Accept(transformer);
        }
    }
}
