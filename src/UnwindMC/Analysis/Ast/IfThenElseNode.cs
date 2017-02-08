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
    }
}
