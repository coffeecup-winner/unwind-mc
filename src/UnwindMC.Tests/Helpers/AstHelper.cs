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
            switch (expected)
            {
                case AssignmentNode assignment:
                    Assert.That(actual, Is.InstanceOf<AssignmentNode>());
                    var actualAssignment = (AssignmentNode)actual;
                    AssertAstEqual(assignment.Var, actualAssignment.Var);
                    AssertAstEqual(assignment.Expression, actualAssignment.Expression);
                    return;
                case BinaryOperatorNode binary:
                    Assert.That(actual, Is.InstanceOf<BinaryOperatorNode>());
                    var actualBinary = (BinaryOperatorNode)actual;
                    Assert.That(actualBinary.Operator, Is.EqualTo(binary.Operator));
                    AssertAstEqual(binary.Left, actualBinary.Left);
                    AssertAstEqual(binary.Right, actualBinary.Right);
                    return;
                case DereferenceNode dereference:
                    Assert.That(actual, Is.InstanceOf<DereferenceNode>());
                    var actualDereference = (DereferenceNode)actual;
                    AssertAstEqual(dereference.Pointer, actualDereference.Pointer);
                    return;
                case DoWhileNode doWhileLoop:
                    Assert.That(actual, Is.InstanceOf<DoWhileNode>());
                    var actualDoWhileLoop = (DoWhileNode)actual;
                    AssertAstEqual(doWhileLoop.Body, actualDoWhileLoop.Body);
                    AssertAstEqual(doWhileLoop.Condition, actualDoWhileLoop.Condition);
                    return;
                case FunctionCallNode call:
                    Assert.That(actual, Is.InstanceOf<FunctionCallNode>());
                    var actualCall = (FunctionCallNode)actual;
                    AssertAstEqual(call.Function, actualCall.Function);
                    return;
                case IfThenElseNode ifThenElse:
                    Assert.That(actual, Is.InstanceOf<IfThenElseNode>());
                    var actualIfThenElse = (IfThenElseNode)actual;
                    AssertAstEqual(ifThenElse.Condition, actualIfThenElse.Condition);
                    AssertAstEqual(ifThenElse.TrueBranch, actualIfThenElse.TrueBranch);
                    AssertAstEqual(ifThenElse.FalseBranch, actualIfThenElse.FalseBranch);
                    return;
                case ReturnNode ret:
                    Assert.That(actual, Is.InstanceOf<ReturnNode>());
                    return;
                case ScopeNode scope:
                    Assert.That(actual, Is.InstanceOf<ScopeNode>());
                    var actualScope = (ScopeNode)actual;
                    Assert.That(actualScope.ChildrenCount, Is.EqualTo(scope.ChildrenCount));
                    foreach (var pair in actualScope.Zip(scope, (a, b) => (a, b)))
                    {
                        AssertAstEqual(pair.Item2, pair.Item1);
                    }
                    return;
                case ValueNode value:
                    Assert.That(actual, Is.InstanceOf<ValueNode>());
                    var actualValue = (ValueNode)actual;
                    Assert.That(actualValue.Value, Is.EqualTo(value.Value));
                    return;
                case VarNode var:
                    Assert.That(actual, Is.InstanceOf<VarNode>());
                    var actualVar = (VarNode)actual;
                    Assert.That(actualVar.Name, Is.EqualTo(var.Name));
                    return;
                case WhileNode whileLoop:
                    Assert.That(actual, Is.InstanceOf<WhileNode>());
                    var actualWhileLoop = (WhileNode)actual;
                    AssertAstEqual(whileLoop.Condition, actualWhileLoop.Condition);
                    AssertAstEqual(whileLoop.Body, actualWhileLoop.Body);
                    return;
                default: throw new NotSupportedException();
            }
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

        public static DoWhileNode DoWhile(ScopeNode body, IExpressionNode condition)
        {
            return new DoWhileNode(body, condition);
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

        public static BinaryOperatorNode Subtract(IExpressionNode left, IExpressionNode right)
        {
            return new BinaryOperatorNode(Operator.Subtract, left, right);
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
