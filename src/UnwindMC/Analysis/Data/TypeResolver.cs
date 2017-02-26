using System;
using System.Collections.Generic;
using System.Linq;
using UnwindMC.Analysis.Flow;
using UnwindMC.Analysis.IL;

namespace UnwindMC.Analysis.Data
{
    public class TypeResolver
    {
        public struct Result
        {
            public readonly IReadOnlyList<Type> ParameterTypes;
            public readonly IReadOnlyList<Type> VariableTypes;

            public Result(IReadOnlyList<Type> parameterTypes, IReadOnlyList<Type> variableTypes)
            {
                ParameterTypes = parameterTypes;
                VariableTypes = variableTypes;
            }
        }

        Dictionary<ILOperand, TypeBuilder>  _types = new Dictionary<ILOperand, TypeBuilder>();
        Dictionary<ILOperand, TypeBuilder> _parameterTypes = new Dictionary<ILOperand, TypeBuilder>();
        Dictionary<ILOperand, int> _currentIds = new Dictionary<ILOperand, int>();
        int _nextId = 0;

        private TypeResolver() { }

        public static Result ResolveTypes(IReadOnlyList<IBlock> blocks)
        {
            return new TypeResolver().ResolveVariableTypes(blocks);
        }

        public Result ResolveVariableTypes(IReadOnlyList<IBlock> blocks)
        {
            List<Type> variableTypes = new List<Type>();
            foreach (var instr in TraverseReversed(blocks))
            {
                switch (instr.Type)
                {
                    case ILInstructionType.Add:
                    case ILInstructionType.Compare:
                    case ILInstructionType.Subtract:
                        if (instr.Source.Type == ILOperandType.Value)
                        {
                            if (!_types.ContainsKey(instr.Target))
                            {
                                AssignTypeBuilder(instr.Target, null);
                            }
                        }
                        else
                        {
                            if (_types.ContainsKey(instr.Source) && _types.ContainsKey(instr.Target))
                            {
                                if (_types[instr.Source] != _types[instr.Target])
                                {
                                    throw new InvalidOperationException("Type mismatch");
                                }
                            }
                            else if (_types.ContainsKey(instr.Source))
                            {
                                AssignTypeBuilder(instr.Target, instr.Source);
                            }
                            else if (_types.ContainsKey(instr.Target))
                            {
                                AssignTypeBuilder(instr.Source, instr.Target);
                            }
                            else
                            {
                                throw new NotImplementedException();
                            }
                        }
                        instr.SetVariableIds(GetCurrentId(_currentIds, instr.Target), GetCurrentId(_currentIds, instr.Source));
                        break;
                    case ILInstructionType.Assign:
                        if (!_types.ContainsKey(instr.Target))
                        {
                            if (instr.Target.Register == NDis86.OperandType.EAX)
                            {
                                var returnValue = ILOperand.FromRegister(NDis86.OperandType.EAX);
                                AssignTypeBuilder(returnValue, null);
                            }
                            else
                            {
                                throw new InvalidOperationException("Assignment appears to not be used in the execution path");
                            }
                        }
                        ILOperand operand;
                        if (instr.Source.Type == ILOperandType.Pointer)
                        {
                            operand = ILOperand.FromRegister(instr.Source.Register);
                        }
                        else
                        {
                            operand = instr.Source;
                        }
                        if (instr.Source.Type == ILOperandType.Value)
                        {
                            if (!_types.ContainsKey(instr.Target))
                            {
                                AssignTypeBuilder(instr.Target, null);
                            }
                        }
                        else
                        {
                            if (_types.ContainsKey(operand))
                            {
                                var type = _types[instr.Target].Build();
                                if (type.IsFunction)
                                {
                                    _types[operand].AddFunctionTrait();
                                }
                                _types[operand].AddIndirectionLevel(type.IndirectionLevel);
                            }
                            else
                            {
                                AssignTypeBuilder(operand, instr.Target);
                            }
                        }
                        if (instr.Source.Type == ILOperandType.Pointer)
                        {
                            _types[operand].AddIndirectionLevel();
                        }
                        instr.SetVariableIds(GetCurrentId(_currentIds, instr.Target), GetCurrentId(_currentIds, operand));
                        while (variableTypes.Count <= instr.TargetId)
                        {
                            variableTypes.Add(null);
                        }
                        variableTypes[instr.TargetId] = _types[instr.Target].Build();
                        _types.Remove(instr.Target);
                        _currentIds.Remove(instr.Target);
                        break;
                    case ILInstructionType.Call:
                        if (!_types.ContainsKey(instr.Target))
                        {
                            AssignTypeBuilder(instr.Target, null);
                        }
                        _types[instr.Target].AddFunctionTrait();
                        instr.SetVariableIds(GetCurrentId(_currentIds, instr.Target), GetCurrentId(_currentIds, instr.Source));
                        break;
                    case ILInstructionType.Return: continue;
                    default: throw new InvalidOperationException("unknown instruction type");
                }
            }
            var parameterTypes = new List<Type>();
            foreach (var pair in _parameterTypes.OrderBy(p => p.Key.Offset))
            {
                parameterTypes.Add(pair.Value.Build());
            }
            return new Result(parameterTypes, variableTypes);
        }

        private void AssignTypeBuilder(ILOperand target, ILOperand source)
        {
            var typeBuilder = source == null ? new TypeBuilder()
                            : source.Type == ILOperandType.Stack ? _parameterTypes[source]
                            : _types[source];
            if (target.Type == ILOperandType.Stack)
            {
                _parameterTypes[target] = typeBuilder;
            }
            else
            {
                _types[target] = typeBuilder;
                _currentIds[target] = _nextId++;
            }
        }

        private static int GetCurrentId(IReadOnlyDictionary<ILOperand, int> currentIds, ILOperand op)
        {
            if (op == null)
            {
                return -1;
            }
            switch (op.Type)
            {
                case ILOperandType.Register: return currentIds[op];
                case ILOperandType.Pointer: return currentIds[op];
                default: return -1;
            }
        }

        private static IEnumerable<ILInstruction> TraverseReversed(IReadOnlyList<IBlock> blocks)
        {
            var stack = new Stack<object>(blocks); // replace with Either
            while (stack.Count > 0)
            {
                var current = stack.Pop();
                var instr = current as ILInstruction;
                if (instr != null)
                {
                    yield return instr;
                    continue;
                }
                var seq = current as SequentialBlock;
                if (seq != null)
                {
                    for (int i = seq.Instructions.Count - 1; i >= 0; i--)
                    {
                        yield return seq.Instructions[i];
                    }
                    continue;
                }
                var whileLoop = current as WhileBlock;
                if (whileLoop != null)
                {
                    stack.Push(whileLoop.Condition);
                    foreach (var child in whileLoop.Children)
                    {
                        stack.Push(child);
                    }
                    continue;
                }
                var doWhileLoop = current as DoWhileBlock;
                if (doWhileLoop != null)
                {
                    foreach (var child in doWhileLoop.Children)
                    {
                        stack.Push(child);
                    }
                    stack.Push(doWhileLoop.Condition);
                    continue;
                }
                var cond = current as ConditionalBlock;
                if (cond != null)
                {
                    stack.Push(cond.Condition);
                    foreach (var child in cond.TrueBranch)
                    {
                        stack.Push(child);
                    }
                    foreach (var child in cond.FalseBranch)
                    {
                        stack.Push(child);
                    }
                    continue;
                }
                throw new InvalidOperationException("Unknown block type");
            }
        }
    }
}
