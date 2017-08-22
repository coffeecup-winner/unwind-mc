using BenchmarkDotNet.Attributes;
using UnwindMC.Generation.Ast;
using static UnwindMC.Generation.Ast.PatternMatching;
using static UnwindMC.Util.PatternMatching;

namespace UnwindMC.Benchmarks
{
    public class PatternMatchingBenchmark
    {
        private readonly AssignmentNode[] _nodes =
        {
            new AssignmentNode(new VarNode("x"), new ValueNode(0)),
            new AssignmentNode(new VarNode("x"), new BinaryOperatorNode(Operator.Or, new VarNode("x"), new ValueNode(0))),
            new AssignmentNode(new VarNode("x"), new BinaryOperatorNode(Operator.And, new VarNode("y"), new ValueNode(0))),
            new AssignmentNode(new VarNode("x"), new BinaryOperatorNode(Operator.And, new VarNode("x"), new ValueNode(1))),
            new AssignmentNode(new VarNode("x"), new BinaryOperatorNode(Operator.And, new VarNode("x"), new ValueNode(0))),
        };
        private AssignmentNode _node;

        [Params(0, 1, 2, 3, 4)]
        public int Index;

        [GlobalSetup]
        public void Setup()
        {
            _node = _nodes[Index];
        }

        [Benchmark(Baseline = true)]
        public IExpressionNode ExplicitConditions()
        {
            if (_node.Expression is BinaryOperatorNode binary && binary.Operator == Operator.And
                && binary.Left is VarNode var && var.Name == _node.Var.Name
                && binary.Right is ValueNode val && val.Value == 0)
            {
                return binary.Right;
            }
            return null;
        }

        [Benchmark]
        public IExpressionNode PatternMatching()
        {
            var v = Capture<ValueNode>();
            return Match(_node.Expression,
                Binary(Var(C(_node.Var.Name)), C(Operator.And), v % Value(C(0)))
                    .Then(() => (ValueNode)v),
                _.Then(() => (ValueNode)null));
        }
    }
}
