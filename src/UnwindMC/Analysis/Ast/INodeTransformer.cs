namespace UnwindMC.Analysis.Ast
{
    public interface INodeTransformer
    {
        AssignmentNode Transform(AssignmentNode node);
        BinaryOperatorNode Transform(BinaryOperatorNode node);
        DereferenceNode Transform(DereferenceNode node);
        DoWhileNode Transform(DoWhileNode node);
        FunctionCallNode Transform(FunctionCallNode node);
        IfThenElseNode Transform(IfThenElseNode node);
        ReturnNode Transform(ReturnNode node);
        ScopeNode Transform(ScopeNode node);
        UnaryOperatorNode Transform(UnaryOperatorNode unaryOperatorNode);
        ValueNode Transform(ValueNode node);
        VarNode Transform(VarNode node);
        WhileNode Transform(WhileNode node);
    }

    public class NodeTransformerBase : INodeTransformer
    {
        public virtual AssignmentNode Transform(AssignmentNode node) => node;
        public virtual BinaryOperatorNode Transform(BinaryOperatorNode node) => node;
        public virtual DereferenceNode Transform(DereferenceNode node) => node;
        public virtual DoWhileNode Transform(DoWhileNode node) => node;
        public virtual FunctionCallNode Transform(FunctionCallNode node) => node;
        public virtual IfThenElseNode Transform(IfThenElseNode node) => node;
        public virtual ReturnNode Transform(ReturnNode node) => node;
        public virtual ScopeNode Transform(ScopeNode node) => node;
        public virtual UnaryOperatorNode Transform(UnaryOperatorNode node) => node;
        public virtual ValueNode Transform(ValueNode node) => node;
        public virtual VarNode Transform(VarNode node) => node;
        public virtual WhileNode Transform(WhileNode node) => node;
    }
}
