namespace UnwindMC.Analysis.Ast
{
    public interface INode
    {
        void Accept(INodeVisitor visitor);
    }

    public static class NodeExtensions
    {
        public static TVisitor Visit<TVisitor>(this INode node, TVisitor visitor)
        where TVisitor : INodeVisitor
        {
            node.Accept(visitor);
            return visitor;
        }
    }
}
