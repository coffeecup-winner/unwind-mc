using System.Collections;
using System.Collections.Generic;

namespace UnwindMC.Generation.Ast
{
    public class ScopeNode : IStatementNode, IEnumerable<IStatementNode>
    {
        private List<IStatementNode> _children;

        public ScopeNode(IStatementNode[] statements = null)
        {
            _children = new List<IStatementNode>(statements ?? new IStatementNode[0]);
        }

        public int ChildrenCount => _children.Count;

        public void Add(IStatementNode node)
        {
            _children.Add(node);
        }

        public void Accept(INodeTransformer transformer)
        {
            var newNode = transformer.Transform(this);
            if (newNode != this)
            {
                _children = newNode._children;
            }
            foreach (var child in _children)
            {
                child.Accept(transformer);
            }
        }

        public IEnumerator<IStatementNode> GetEnumerator()
        {
            return _children.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return ((IEnumerable<IStatementNode>)_children).GetEnumerator();
        }
    }
}
