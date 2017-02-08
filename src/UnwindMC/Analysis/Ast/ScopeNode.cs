using System.Collections;
using System.Collections.Generic;

namespace UnwindMC.Analysis.Ast
{
    public class ScopeNode : IStatementNode, IEnumerable<IStatementNode>
    {
        private readonly List<IStatementNode> _children;

        public ScopeNode(IStatementNode[] statements = null)
        {
            _children = new List<IStatementNode>(statements ?? new IStatementNode[0]);
        }

        public int ChildrenCount => _children.Count;

        public void Add(IStatementNode node)
        {
            _children.Add(node);
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
