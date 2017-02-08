namespace UnwindMC.Analysis.Ast
{
    public class VarNode : IExpressionNode
    {
        private string _name;

        public VarNode(string name)
        {
            _name = name;
        }

        public string Name => _name;
    }
}
