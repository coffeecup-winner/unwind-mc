using BenchmarkDotNet.Attributes;
using UnwindMC.Generation.Ast;
using static UnwindMC.Generation.Ast.PatternMatching;
using static UnwindMC.Util.PatternMatching;

namespace UnwindMC.Benchmarks
{
    public class PatternMatchingBenchmark2
    {
        private readonly BinaryOperatorNode[] _nodes =
        {
            new BinaryOperatorNode(Operator.Or, new VarNode("x"), new ValueNode(0)),
            new BinaryOperatorNode(Operator.Add, new ValueNode(0), new ValueNode(0)),
            new BinaryOperatorNode(Operator.Add, new VarNode("x"), new ValueNode(0)),
            new BinaryOperatorNode(Operator.Add, new ValueNode(1), new VarNode("x")),
            new BinaryOperatorNode(Operator.Subtract, new VarNode("x"), new ValueNode(0)),
            new BinaryOperatorNode(Operator.Subtract, new ValueNode(0), new VarNode("x")),
        };
        private BinaryOperatorNode _node;

        [Params(0, 1, 2, 3, 4, 5)]
        public int Index;

        [GlobalSetup]
        public void Setup()
        {
            _node = _nodes[Index];
        }

        [Benchmark(Baseline = true)]
        public IExpressionNode ExplicitConditions()
        {
            if (_node.Operator != Operator.Add && _node.Operator != Operator.Subtract)
            {
                return _node;
            }
            VarNode var = _node.Left as VarNode;
            if (var == null)
            {
                var = _node.Right as VarNode;
                if (var == null)
                {
                    return _node;
                }
            }
            bool isVarLeft = _node.Left == var;
            ValueNode value = (isVarLeft ? _node.Right : _node.Left) as ValueNode;
            if (value == null)
            {
                return _node;
            }
            return _node;
        }

        [Benchmark]
        public IExpressionNode PatternMatching()
        {
            var var = Capture<VarNode>();
            var value = Capture<ValueNode>();
            return Match(_node,
                Binary(var % Var(_), C(Operator.Add), value % Value(_)).Then(() => Fixup(_node, var, value)),
                Binary(value % Value(_), C(Operator.Add), var % Var(_)).Then(() => Fixup(_node, value, var)),
                Binary(var % Var(_), C(Operator.Subtract), value % Value(_)).Then(() => Fixup(_node, var, value)),
                Binary(value % Value(_), C(Operator.Subtract), var % Var(_)).Then(() => Fixup(_node, value, var)),
                Otherwise(_node));
        }

        private BinaryOperatorNode Fixup(BinaryOperatorNode node, VarNode var, ValueNode value)
        {
            return node;
        }

        private BinaryOperatorNode Fixup(BinaryOperatorNode node, ValueNode value, VarNode var)
        {
            return Fixup(node, var, value);
        }
    }
}
