﻿namespace UnwindMC.Analysis.Ast
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
    }
}
