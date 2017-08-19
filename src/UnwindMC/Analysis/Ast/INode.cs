namespace UnwindMC.Analysis.Ast
{
    public interface INode
    {
        void Accept(INodeTransformer transformer);
    }

    public static class NodeExtensions
    {
        public static TTransformer Transform<TTransformer>(this INode node, TTransformer transformer)
        where TTransformer : INodeTransformer
        {
            node.Accept(transformer);
            return transformer;
        }
    }
}
