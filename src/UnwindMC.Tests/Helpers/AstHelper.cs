using NUnit.Framework;
using System;
using System.Linq;
using UnwindMC.Analysis.Ast;

namespace UnwindMC.Tests.Helpers
{
    public static class AstHelper
    {
        public static void AssertAstEqual(INode expected, INode actual)
        {
            var assignment = expected as AssignmentNode;
            if (assignment != null)
            {
                Assert.That(actual, Is.InstanceOf<AssignmentNode>());
                var actualAssignment = (AssignmentNode)actual;
                AssertAstEqual(assignment.Var, actualAssignment.Var);
                AssertAstEqual(assignment.Expression, actualAssignment.Expression);
                return;
            }
            var binary = expected as BinaryOperatorNode;
            if (binary != null)
            {
                Assert.That(actual, Is.InstanceOf<BinaryOperatorNode>());
                var actualBinary = (BinaryOperatorNode)actual;
                Assert.That(actualBinary.Operator, Is.EqualTo(binary.Operator));
                AssertAstEqual(binary.Left, actualBinary.Left);
                AssertAstEqual(binary.Right, actualBinary.Right);
                return;
            }
            var dereference = expected as DereferenceNode;
            if (dereference != null)
            {
                Assert.That(actual, Is.InstanceOf<DereferenceNode>());
                var actualDereference = (DereferenceNode)actual;
                AssertAstEqual(dereference.Pointer, actualDereference.Pointer);
                return;
            }
            var call = expected as FunctionCallNode;
            if (call != null)
            {
                Assert.That(actual, Is.InstanceOf<FunctionCallNode>());
                var actualCall = (FunctionCallNode)actual;
                AssertAstEqual(call.Function, actualCall.Function);
                return;
            }
            var ifThenElse = expected as IfThenElseNode;
            if (ifThenElse != null)
            {
                Assert.That(actual, Is.InstanceOf<IfThenElseNode>());
                var actualIfThenElse = (IfThenElseNode)actual;
                AssertAstEqual(ifThenElse.Condition, actualIfThenElse.Condition);
                AssertAstEqual(ifThenElse.TrueBranch, actualIfThenElse.TrueBranch);
                AssertAstEqual(ifThenElse.FalseBranch, actualIfThenElse.FalseBranch);
                return;
            }
            var ret = expected as ReturnNode;
            if (ret != null)
            {
                Assert.That(actual, Is.InstanceOf<ReturnNode>());
                return;
            }
            var scope = expected as ScopeNode;
            if (scope != null)
            {
                Assert.That(actual, Is.InstanceOf<ScopeNode>());
                var actualScope = (ScopeNode)actual;
                Assert.That(actualScope.ChildrenCount, Is.EqualTo(scope.ChildrenCount));
                foreach (var pair in actualScope.Zip(scope, (a, b) => Tuple.Create(a, b)))
                {
                    AssertAstEqual(pair.Item2, pair.Item1);
                }
                return;
            }
            var value = expected as ValueNode;
            if (value != null)
            {
                Assert.That(actual, Is.InstanceOf<ValueNode>());
                var actualValue = (ValueNode)actual;
                Assert.That(actualValue.Value, Is.EqualTo(value.Value));
                return;
            }
            var var = expected as VarNode;
            if (var != null)
            {
                Assert.That(actual, Is.InstanceOf<VarNode>());
                var actualVar = (VarNode)actual;
                Assert.That(actualVar.Name, Is.EqualTo(var.Name));
                return;
            }
            var whileLoop = expected as WhileNode;
            if (whileLoop != null)
            {
                Assert.That(actual, Is.InstanceOf<WhileNode>());
                var actualWhileLoop = (WhileNode)actual;
                AssertAstEqual(whileLoop.Condition, actualWhileLoop.Condition);
                AssertAstEqual(whileLoop.Body, actualWhileLoop.Body);
                return;
            }
            throw new NotSupportedException();
        }

        public static BinaryOperatorNode Add(IExpressionNode left, IExpressionNode right)
        {
            return new BinaryOperatorNode(Operator.Add, left, right);
        }

        public static AssignmentNode Assign(VarNode var, IExpressionNode expression)
        {
            return new AssignmentNode(var, expression);
        }

        public static FunctionCallNode Call(IExpressionNode function)
        {
            return new FunctionCallNode(function);
        }

        public static DereferenceNode Dereference(IExpressionNode pointer)
        {
            return new DereferenceNode(pointer);
        }

        public static IfThenElseNode IfThenElse(IExpressionNode condition, ScopeNode trueBranch, ScopeNode falseBranch)
        {
            return new IfThenElseNode(condition, trueBranch, falseBranch);
        }

        public static BinaryOperatorNode Less(IExpressionNode left, IExpressionNode right)
        {
            return new BinaryOperatorNode(Operator.Less, left, right);
        }

        public static BinaryOperatorNode NotEqual(IExpressionNode left, IExpressionNode right)
        {
            return new BinaryOperatorNode(Operator.NotEqual, left, right);
        }

        public static ReturnNode Ret()
        {
            return new ReturnNode();
        }

        public static ScopeNode Scope(params IStatementNode[] statements)
        {
            return new ScopeNode(statements);
        }

        public static ValueNode Val(int value)
        {
            return new ValueNode(value);
        }

        public static VarNode Var(string name)
        {
            return new VarNode(name);
        }

        public static WhileNode While(IExpressionNode condition, ScopeNode body)
        {
            return new WhileNode(condition, body);
        }
    }
}
