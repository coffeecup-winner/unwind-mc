namespace UnwindMC.Analysis.Ast
{
    public class AssignmentNode : IStatementNode
    {
        private VarNode _var;

        public AssignmentNode(VarNode var, IExpressionNode expression)
        {
            _var = var;
            Expression = expression;
        }

        public VarNode Var => _var;
        // TODO: make nodes editable only from inside visitor
        public IExpressionNode Expression { get; set; }

        public void Accept(INodeVisitor visitor)
        {
            visitor.Visit(this);
            _var.Accept(visitor);
            Expression.Accept(visitor);
        }
    }
}
