namespace UnwindMC.Analysis.Ast
{
    public class ReturnNode : IStatementNode
    {
        public void Accept(INodeVisitor visitor)
        {
            visitor.Visit(this);
        }
    }
}
