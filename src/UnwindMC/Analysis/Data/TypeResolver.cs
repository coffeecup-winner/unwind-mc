using System;
using System.Collections.Generic;
using System.Linq;
using UnwindMC.Analysis.Flow;
using UnwindMC.Analysis.IL;
using UnwindMC.Util;

namespace UnwindMC.Analysis.Data
{
    public class TypeResolver
    {
        private enum LoopBoundsMarker
        {
            Start,
            End,
        }

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
            var variableTypes = new List<Type>();
            var typesToRemove = new Dictionary<int, Dictionary<ILOperand, int>>
            {
                { 0, new Dictionary<ILOperand, int>() }
            };
            int currentLevel = 0;
            foreach (var item in TraverseReversed(blocks))
            {
                if (item.IsLeft)
                {
                    switch (item.Left)
                    {
                        case LoopBoundsMarker.Start:
                            foreach (var pair in typesToRemove[currentLevel])
                            {
                                Remove(variableTypes, pair.Value, pair.Key);
                            }
                            typesToRemove.Remove(currentLevel);
                            currentLevel--;
                            break;
                        case LoopBoundsMarker.End:
                            currentLevel++;
                            typesToRemove[currentLevel] = new Dictionary<ILOperand, int>();
                            break;
                        default: throw new NotSupportedException();
                    }
                    continue;
                }
                var instr = item.Right;
                switch (instr.Type)
                {
                    case ILInstructionType.Add:
                    case ILInstructionType.Compare:
                    case ILInstructionType.Subtract:
                        if (typesToRemove[currentLevel].ContainsKey(instr.Target))
                        {
                            typesToRemove[currentLevel].Remove(instr.Target);
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
                        if (currentLevel == 0)
                        {
                            Remove(variableTypes, instr.TargetId, instr.Target);
                        }
                        else
                        {
                            typesToRemove[currentLevel].Add(instr.Target, instr.TargetId);
                        }
                        break;
                    case ILInstructionType.Call:
                        if (typesToRemove[currentLevel].ContainsKey(instr.Target))
                        {
                            typesToRemove[currentLevel].Remove(instr.Target);
                        }
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

        private void Remove(List<Type> variableTypes, int id, ILOperand operand)
        {
            while (variableTypes.Count <= id)
            {
                variableTypes.Add(null);
            }
            variableTypes[id] = _types[operand].Build();
            _types.Remove(operand);
            _currentIds.Remove(operand);
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

        private static IEnumerable<Either<LoopBoundsMarker, ILInstruction>> TraverseReversed(IReadOnlyList<IBlock> blocks)
        {
            var stack = new Stack<object>(blocks); // replace with Either
            while (stack.Count > 0)
            {
                var current = stack.Pop();
                var loopBoundsMarker = current as LoopBoundsMarker?;
                if (loopBoundsMarker != null)
                {
                    yield return Either.Left<LoopBoundsMarker, ILInstruction>(loopBoundsMarker.Value);
                    continue;
                }
                var instr = current as ILInstruction;
                if (instr != null)
                {
                    yield return Either.Right<LoopBoundsMarker, ILInstruction>(instr);
                    continue;
                }
                var seq = current as SequentialBlock;
                if (seq != null)
                {
                    for (int i = seq.Instructions.Count - 1; i >= 0; i--)
                    {
                        yield return Either.Right<LoopBoundsMarker, ILInstruction>(seq.Instructions[i]);
                    }
                    continue;
                }
                var whileLoop = current as WhileBlock;
                if (whileLoop != null)
                {
                    stack.Push(LoopBoundsMarker.Start);
                    stack.Push(whileLoop.Condition);
                    foreach (var child in whileLoop.Children)
                    {
                        stack.Push(child);
                    }
                    stack.Push(LoopBoundsMarker.End);
                    continue;
                }
                var doWhileLoop = current as DoWhileBlock;
                if (doWhileLoop != null)
                {
                    stack.Push(LoopBoundsMarker.Start);
                    foreach (var child in doWhileLoop.Children)
                    {
                        stack.Push(child);
                    }
                    stack.Push(doWhileLoop.Condition);
                    stack.Push(LoopBoundsMarker.End);
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
