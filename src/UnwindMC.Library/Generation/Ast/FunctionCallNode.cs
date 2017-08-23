namespace UnwindMC.Generation.Ast
{
    public class FunctionCallNode : IStatementNode
    {
        private IExpressionNode _function;

        public FunctionCallNode(IExpressionNode function)
        {
            _function = function;
        }

        public IExpressionNode Function => _function;

        public void Accept(INodeTransformer transformer)
        {
            var newNode = transformer.Transform(this);
            if (newNode != this)
            {
                _function = newNode._function;
            }
            _function.Accept(transformer);
        }
    }
}
