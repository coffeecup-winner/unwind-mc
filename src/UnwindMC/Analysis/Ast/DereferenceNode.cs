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

        public void Accept(INodeTransformer transformer)
        {
            var newNode = transformer.Transform(this);
            if (newNode != this)
            {
                _pointer = newNode._pointer;
            }
            _pointer.Accept(transformer);
        }
    }
}
