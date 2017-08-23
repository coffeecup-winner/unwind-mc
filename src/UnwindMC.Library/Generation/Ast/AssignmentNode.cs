namespace UnwindMC.Generation.Ast
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

        public void Accept(INodeTransformer transformer)
        {
            var newNode = transformer.Transform(this);
            if (newNode != this)
            {
                _var = newNode._var;
                _expression = newNode._expression;
            }
            _var.Accept(transformer);
            Expression.Accept(transformer);
        }
    }
}
