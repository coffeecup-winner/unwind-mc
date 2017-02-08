using System;
using System.Collections.Generic;
using UnwindMC.Analysis.Flow;
using UnwindMC.Analysis.IL;

namespace UnwindMC.Analysis.Ast
{
    public class AstBuilder
    {
        private readonly IReadOnlyList<IBlock> _blocks;
        private readonly IReadOnlyDictionary<ILOperand, Data.Type> _arguments;

        public AstBuilder(IReadOnlyList<IBlock> blocks, IReadOnlyDictionary<ILOperand, Data.Type> arguments)
        {
            _blocks = blocks;
            _arguments = arguments;
        }

        public ScopeNode BuildAst()
        {
            return BuildScope(_blocks);
        }

        private ScopeNode BuildScope(IReadOnlyList<IBlock> blocks)
        {
            var scope = new ScopeNode();
            foreach (var block in blocks)
            {
                var seq = block as SequentialBlock;
                if (seq != null)
                {
                    foreach (var instr in seq.Instructions)
                    {
                        scope.Add(BuildStatement(instr));
                    }
                    continue;
                }
                var loop = block as LoopBlock;
                if (loop != null)
                {
                    scope.Add(BuildLoop(loop));
                    continue;
                }
                var cond = block as ConditionalBlock;
                if (cond != null)
                {
                    scope.Add(BuildIfThenElse(cond));
                }
            }
            return scope;
        }

        private IStatementNode BuildLoop(LoopBlock loop)
        {
            return new WhileNode(BuildExpression(loop.Condition), BuildScope(loop.Children));
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
                    return new AssignmentNode(BuildVar(instr.Target), new BinaryOperatorNode(Operator.Add, BuildExpression(instr.Target), BuildExpression(instr.Source)));
                case ILInstructionType.Assign:
                    return new AssignmentNode(BuildVar(instr.Target), BuildExpression(instr.Source));
                case ILInstructionType.Call:
                    return new FunctionCallNode(BuildExpression(instr.Target));
                case ILInstructionType.Return:
                    return new ReturnNode();
                default: throw new ArgumentException("Instruction is not a valid statement");
            }
        }

        private IExpressionNode BuildExpression(ILInstruction instr)
        {
            switch (instr.Type)
            {
                case ILInstructionType.Compare:
                    return new BinaryOperatorNode(GetBinaryOperator(instr.Condition), BuildExpression(instr.Target), BuildExpression(instr.Source));
                default: throw new ArgumentException("Instruction is not a valid statement");
            }
        }

        private IExpressionNode BuildExpression(ILOperand op)
        {
            switch (op.Type)
            {
                case ILOperandType.Pointer: return new DereferenceNode(new VarNode("var"));
                case ILOperandType.Register: return new VarNode("var");
                case ILOperandType.Stack: return new VarNode("arg");
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

        private VarNode BuildVar(ILOperand op)
        {
            switch (op.Type)
            {
                case ILOperandType.Register: return new VarNode("var");
                default: throw new NotSupportedException();
            }
        }
    }
}
