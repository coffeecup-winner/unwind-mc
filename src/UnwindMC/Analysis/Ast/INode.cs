namespace UnwindMC.Analysis.Ast
{
    public interface INode
    {
        void Accept(INodeVisitor visitor);
    }
}
