namespace UnwindMC.Generation.Ast
{
    public class VarNode : IExpressionNode
    {
        private string _name;

        public VarNode(string name)
        {
            _name = name;
        }

        public string Name => _name;

        public void Accept(INodeTransformer transformer)
        {
            var newNode = transformer.Transform(this);
            if (newNode != this)
            {
                _name = newNode._name;
            }
        }
    }
}
