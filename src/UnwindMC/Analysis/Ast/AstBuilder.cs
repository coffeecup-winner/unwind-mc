using System;
using System.Collections.Generic;
using UnwindMC.Analysis.Ast.Transformations;
using UnwindMC.Analysis.Flow;
using UnwindMC.Analysis.IL;
using UnwindMC.Util;

namespace UnwindMC.Analysis.Ast
{
    public class AstBuilder
    {
        private readonly IReadOnlyList<IBlock> _blocks;
        private readonly IReadOnlyList<Data.Type> _parameterTypes;
        private readonly IReadOnlyList<Data.Type> _variableTypes;

        private int _nextVariableNameIdx;
        private Dictionary<int, string> _variableNames;
        private Dictionary<int, string> _parameterNames;
        private Dictionary<string, Data.Type> _types;
        
        public AstBuilder(IReadOnlyList<IBlock> blocks, IReadOnlyList<Data.Type> parameterTypes, IReadOnlyList<Data.Type> variableTypes)
        {
            _blocks = blocks;
            _parameterTypes = parameterTypes;
            _variableTypes = variableTypes;
        }

        public ScopeNode BuildAst()
        {
            _nextVariableNameIdx = 0;
            _variableNames = new Dictionary<int, string>();
            _parameterNames = new Dictionary<int, string>();
            _types = new Dictionary<string, Data.Type>();
            int offset = 0;
            for (int index = 0; index < _parameterTypes.Count; index++)
            {
                var name = "arg" + index;
                _parameterNames[offset] = name;
                _types[name] = _parameterTypes[index];
                offset += _parameterTypes[index].Size;
            }
            var ast = BuildScope(_blocks);
            RunTransformations(ast);
            return ast;
        }

        private void RunTransformations(ScopeNode ast)
        {
            var transformers = new INodeVisitor[]
            {
                new FixupPointerArithmetics(_types),
            };

            foreach (var transformer in transformers)
            {
                ast.Accept(transformer);
            }
        }

        private ScopeNode BuildScope(IReadOnlyList<IBlock> blocks)
        {
            var scope = new ScopeNode();
            foreach (var block in blocks)
            {
                switch (block)
                {
                    case SequentialBlock seq:
                        foreach (var instr in seq.Instructions)
                        {
                            scope.Add(BuildStatement(instr));
                        }
                        break;
                    case WhileBlock whileLoop:
                        scope.Add(BuildWhile(whileLoop));
                        break;
                    case DoWhileBlock doWhileLoop:
                        scope.Add(BuildDoWhile(doWhileLoop));
                        break;
                    case ConditionalBlock cond:
                        scope.Add(BuildIfThenElse(cond));
                        break;
                    default: throw new NotSupportedException();
                }
            }
            return scope;
        }

        private IStatementNode BuildWhile(WhileBlock loop)
        {
            return new WhileNode(BuildExpression(loop.Condition), BuildScope(loop.Children));
        }

        private IStatementNode BuildDoWhile(DoWhileBlock doWhileLoop)
        {
            return new DoWhileNode(BuildScope(doWhileLoop.Children), BuildExpression(doWhileLoop.Condition));
        }

        private IfThenElseNode BuildIfThenElse(ConditionalBlock cond)
        {
            return new IfThenElseNode(BuildExpression(cond.Condition), BuildScope(cond.TrueBranch), BuildScope(cond.FalseBranch));
        }

        private IStatementNode BuildStatement(ILInstruction instr)
        {
            switch (instr.Type)
            {
                case ILInstructionType.Add:
                    return new AssignmentNode(BuildVar(instr.Target, instr.TargetId),
                        BuildBinaryOperator(Operator.Add, instr));
                case ILInstructionType.Assign:
                    return new AssignmentNode(BuildVar(instr.Target, instr.TargetId), BuildExpression(instr.Source, instr.SourceId));
                case ILInstructionType.Call:
                    return new FunctionCallNode(BuildExpression(instr.Target, instr.TargetId));
                case ILInstructionType.Divide:
                    return new AssignmentNode(BuildVar(instr.Target, instr.TargetId),
                        BuildBinaryOperator(Operator.Divide, instr));
                case ILInstructionType.Multiply:
                    return new AssignmentNode(BuildVar(instr.Target, instr.TargetId),
                        BuildBinaryOperator(Operator.Multiply, instr));
                case ILInstructionType.Negate:
                    return new AssignmentNode(BuildVar(instr.Target, instr.TargetId),
                        BuildUnaryOperator(Operator.Negate, instr));
                case ILInstructionType.Return:
                    return new ReturnNode(instr.SourceId == -1 ? Option<VarNode>.None : Option.Some(BuildVar(instr.Source, instr.SourceId)));
                case ILInstructionType.Subtract:
                    return new AssignmentNode(BuildVar(instr.Target, instr.TargetId),
                        BuildBinaryOperator(Operator.Subtract, instr));
                default: throw new ArgumentException("Instruction is not a valid statement");
            }
        }

        private IExpressionNode BuildExpression(ILInstruction instr)
        {
            switch (instr.Type)
            {
                case ILInstructionType.Compare:
                    return BuildBinaryOperator(GetBinaryOperator(instr.Condition), instr);
                default: throw new ArgumentException("Instruction is not a valid statement");
            }
        }

        private IExpressionNode BuildExpression(ILOperand op, int id)
        {
            switch (op.Type)
            {
                case ILOperandType.Pointer: return new DereferenceNode(new VarNode(GetVarName(id)));
                case ILOperandType.Register: return new VarNode(GetVarName(id));
                case ILOperandType.Stack: return new VarNode(_parameterNames[op.Offset]);
                case ILOperandType.Value: return new ValueNode(op.Value);
                default: throw new InvalidOperationException();
            }
        }

        private Operator GetBinaryOperator(ILBranchType condition)
        {
            switch (condition)
            {
                case ILBranchType.Equal: return Operator.Equal;
                case ILBranchType.NotEqual: return Operator.NotEqual;
                case ILBranchType.Less: return Operator.Less;
                case ILBranchType.LessOrEqual: return Operator.GreaterOrEqual;
                case ILBranchType.GreaterOrEqual: return Operator.GreaterOrEqual;
                case ILBranchType.Greater: return Operator.Greater;
                case ILBranchType.Next: throw new InvalidOperationException("Next is not a valid operator");
                default: throw new InvalidOperationException();
            }
        }

        private BinaryOperatorNode BuildBinaryOperator(Operator op, ILInstruction instr)
        {
            return new BinaryOperatorNode(op,
                BuildExpression(instr.Target, instr.TargetId),
                BuildExpression(instr.Source, instr.SourceId));
        }

        private UnaryOperatorNode BuildUnaryOperator(Operator op, ILInstruction instr)
        {
            return new UnaryOperatorNode(op,
                BuildExpression(instr.Target, instr.TargetId));
        }

        private VarNode BuildVar(ILOperand op, int id)
        {
            switch (op.Type)
            {
                case ILOperandType.Register: return new VarNode(GetVarName(id));
                default: throw new NotSupportedException();
            }
        }

        private string GetVarName(int id)
        {
            if (id == -1)
            {
                throw new InvalidOperationException("Invalid id");
            }
            if (!_variableNames.TryGetValue(id, out string name))
            {
                name = "var" + _nextVariableNameIdx++;
                _variableNames[id] = name;
                _types[name] = _variableTypes[id];
            }
            return name;
        }
    }
}
