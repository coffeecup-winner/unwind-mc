namespace UnwindMC.Analysis.Ast
{
    public class DereferenceNode : IExpressionNode
    {
        private IExpressionNode _pointer;

        public DereferenceNode(IExpressionNode pointer)
        {
            _pointer = pointer;
        }

        public IExpressionNode Pointer => _pointer;

        public void Accept(INodeVisitor visitor)
        {
            visitor.Visit(this);
            _pointer.Accept(visitor);
        }
    }
}
