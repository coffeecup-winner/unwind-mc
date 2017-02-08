namespace UnwindMC.Analysis.Ast
{
    public class AssignmentNode : IStatementNode
    {
        private VarNode _var;
        private IExpressionNode _expression;

        public AssignmentNode(VarNode var, IExpressionNode expression)
        {
            _var = var;
            _expression = expression;
        }

        public VarNode Var => _var;
        public IExpressionNode Expression => _expression;
    }
}
