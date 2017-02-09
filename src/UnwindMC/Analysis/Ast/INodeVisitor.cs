using System;

namespace UnwindMC.Analysis.Ast
{
    public interface INodeVisitor
    {
        void Visit(AssignmentNode node);
        void Visit(BinaryOperatorNode node);
        void Visit(DereferenceNode node);
        void Visit(FunctionCallNode node);
        void Visit(IfThenElseNode node);
        void Visit(ReturnNode node);
        void Visit(ScopeNode node);
        void Visit(ValueNode node);
        void Visit(VarNode node);
        void Visit(WhileNode node);
    }

    public class NodeVisitorBase : INodeVisitor
    {
        public virtual void Visit(DereferenceNode node) { }
        public virtual void Visit(IfThenElseNode node) { }
        public virtual void Visit(ScopeNode node) { }
        public virtual void Visit(VarNode node) { }
        public virtual void Visit(WhileNode node) { }
        public virtual void Visit(ValueNode node) { }
        public virtual void Visit(ReturnNode node) { }
        public virtual void Visit(FunctionCallNode node) { }
        public virtual void Visit(BinaryOperatorNode node) { }
        public virtual void Visit(AssignmentNode node) { }
    }
}
