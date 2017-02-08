﻿namespace UnwindMC.Analysis.Ast
{
    public class FunctionCallNode : IStatementNode
    {
        private IExpressionNode _function;

        public FunctionCallNode(IExpressionNode function)
        {
            _function = function;
        }

        public IExpressionNode Function => _function;
    }
}