using System;
using System.Collections.Generic;
using System.Linq;
using NDis86;
using UnwindMC.Analysis.Flow;
using UnwindMC.Analysis.IL;
using UnwindMC.Util;

namespace UnwindMC.Analysis.Data
{
    public class TypeResolver
    {
        private enum ScopeBoundsMarker
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

        private readonly Dictionary<int, Dictionary<ILOperand, TypeBuilder>> _types = new Dictionary<int, Dictionary<ILOperand, TypeBuilder>>();
        private readonly Dictionary<ILOperand, TypeBuilder> _parameterTypes = new Dictionary<ILOperand, TypeBuilder>();
        private readonly Dictionary<ILOperand, int> _currentIds = new Dictionary<ILOperand, int>();
        private int _currentLevel;
        private int _nextId;

        private TypeResolver() { }

        public static Result ResolveTypes(IReadOnlyList<IBlock> blocks)
        {
            return new TypeResolver().ResolveVariableTypes(blocks);
        }

        private Result ResolveVariableTypes(IReadOnlyList<IBlock> blocks)
        {
            var variableTypes = new List<Type>();
            var typesToRemove = new Dictionary<int, Dictionary<ILOperand, int>>
            {
                { 0, new Dictionary<ILOperand, int>() }
            };
            _types[_currentLevel] = new Dictionary<ILOperand, TypeBuilder>();
            foreach (var item in TraverseReversed(blocks))
            {
                if (item.IsLeft)
                {
                    switch (item.Left)
                    {
                        case ScopeBoundsMarker.Start:
                            foreach (var pair in typesToRemove[_currentLevel])
                            {
                                if (GetScopeLevel(pair.Key) == _currentLevel)
                                {
                                    FinalizeType(variableTypes, pair.Value, pair.Key);
                                }
                            }
                            typesToRemove.Remove(_currentLevel);
                            foreach (var pair in _types[_currentLevel])
                            {
                                _types[_currentLevel - 1].Add(pair.Key, pair.Value);
                            }
                            _types.Remove(_currentLevel);
                            _currentLevel--;
                            break;
                        case ScopeBoundsMarker.End:
                            _currentLevel++;
                            _types[_currentLevel] = new Dictionary<ILOperand, TypeBuilder>();
                            typesToRemove[_currentLevel] = new Dictionary<ILOperand, int>();
                            break;
                        default: throw new NotSupportedException();
                    }
                    continue;
                }
                var instr = item.Right;
                switch (instr.Type)
                {
                    case ILInstructionType.Negate:
                    case ILInstructionType.Not:
                        instr.SetVariableIds(GetCurrentId(_currentIds, instr.Target), GetCurrentId(_currentIds, instr.Source));
                        break;
                    case ILInstructionType.Add:
                    case ILInstructionType.And:
                    case ILInstructionType.Compare:
                    case ILInstructionType.Divide:
                    case ILInstructionType.Multiply:
                    case ILInstructionType.Or:
                    case ILInstructionType.ShiftLeft:
                    case ILInstructionType.ShiftRight:
                    case ILInstructionType.Subtract:
                    case ILInstructionType.Xor:
                        if (typesToRemove[_currentLevel].ContainsKey(instr.Target))
                        {
                            typesToRemove[_currentLevel].Remove(instr.Target);
                        }
                        if (instr.Source.Type == ILOperandType.Value)
                        {
                            if (!TypeExists(instr.Target))
                            {
                                AssignTypeBuilder(instr.Target, null);
                            }
                        }
                        else
                        {
                            if (TypeExists(instr.Source) && TypeExists(instr.Target))
                            {
                                if (GetType(instr.Source) != GetType(instr.Target))
                                {
                                    throw new InvalidOperationException("Type mismatch");
                                }
                            }
                            else if (TypeExists(instr.Source))
                            {
                                AssignTypeBuilder(instr.Target, instr.Source);
                            }
                            else if (TypeExists(instr.Target))
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
                        if (!TypeExists(instr.Target))
                        {
                            throw new InvalidOperationException("Assignment appears to not be used in the execution path");
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
                            if (!TypeExists(instr.Target))
                            {
                                AssignTypeBuilder(instr.Target, null);
                            }
                        }
                        else
                        {
                            if (TypeExists(operand))
                            {
                                var type = GetType(instr.Target).Build();
                                if (type.IsFunction)
                                {
                                    GetType(operand).AddFunctionTrait();
                                }
                                GetType(operand).AddIndirectionLevel(type.IndirectionLevel);
                            }
                            else
                            {
                                AssignTypeBuilder(operand, instr.Target);
                            }
                        }
                        if (instr.Source.Type == ILOperandType.Pointer)
                        {
                            GetType(operand).AddIndirectionLevel();
                        }
                        instr.SetVariableIds(GetCurrentId(_currentIds, instr.Target), GetCurrentId(_currentIds, operand));
                        if (_currentLevel == 0)
                        {
                            FinalizeType(variableTypes, instr.TargetId, instr.Target);
                        }
                        else
                        {
                            typesToRemove[_currentLevel].Add(instr.Target, instr.TargetId);
                        }
                        break;
                    case ILInstructionType.Call:
                        if (typesToRemove[_currentLevel].ContainsKey(instr.Target))
                        {
                            typesToRemove[_currentLevel].Remove(instr.Target);
                        }
                        if (!TypeExists(instr.Target))
                        {
                            AssignTypeBuilder(instr.Target, null);
                        }
                        GetType(instr.Target).AddFunctionTrait();
                        instr.SetVariableIds(GetCurrentId(_currentIds, instr.Target), GetCurrentId(_currentIds, instr.Source));
                        break;
                    case ILInstructionType.Return:
                        if (FunctionReturnsValue(blocks))
                        {
                            AssignTypeBuilder(instr.Source, null);
                            instr.SetVariableIds(GetCurrentId(_currentIds, instr.Target), GetCurrentId(_currentIds, instr.Source));
                        }
                        break;
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

        private static bool FunctionReturnsValue(IReadOnlyList<IBlock> blocks)
        {
            // This tests only the top-level scope, which is probably an incomplete heuristic,
            // most probably need to check all paths
            return blocks
                .OfType<SequentialBlock>()
                .SelectMany(b => b.Instructions)
                .Any(i => i.Target != null && i.Target.Type == ILOperandType.Register && i.Target.Register == OperandType.EAX);
        }

        private bool TypeExists(ILOperand operand)
        {
            for (int i = _currentLevel; i >= 0; i--)
            {
                if (_types[i].ContainsKey(operand))
                {
                    return true;
                }
            }
            return false;
        }

        private int GetScopeLevel(ILOperand operand)
        {
            for (int i = _currentLevel; i >= 0; i--)
            {
                if (_types[i].ContainsKey(operand))
                {
                    return i;
                }
            }
            throw new KeyNotFoundException();
        }

        private TypeBuilder GetType(ILOperand operand)
        {
            for (int i = _currentLevel; i >= 0; i--)
            {
                if (_types[i].ContainsKey(operand))
                {
                    return _types[i][operand];
                }
            }
            throw new KeyNotFoundException();
        }

        private void FinalizeType(List<Type> variableTypes, int id, ILOperand operand)
        {
            while (variableTypes.Count <= id)
            {
                variableTypes.Add(null);
            }
            variableTypes[id] = _types[_currentLevel][operand].Build();
            _types[_currentLevel].Remove(operand);
            _currentIds.Remove(operand);
        }

        private void AssignTypeBuilder(ILOperand target, ILOperand source)
        {
            var typeBuilder = source == null ? new TypeBuilder()
                            : source.Type == ILOperandType.Stack ? _parameterTypes[source]
                            : GetType(source);
            if (target.Type == ILOperandType.Stack)
            {
                _parameterTypes[target] = typeBuilder;
            }
            else
            {
                _types[_currentLevel][target] = typeBuilder;
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

        private static IEnumerable<Either<ScopeBoundsMarker, ILInstruction>> TraverseReversed(IReadOnlyList<IBlock> blocks)
        {
            var stack = new Stack<object>(blocks); // replace with Either
            while (stack.Count > 0)
            {
                switch (stack.Pop())
                {
                    case ScopeBoundsMarker scopeBoundsMarker:
                        yield return Either.Left<ScopeBoundsMarker, ILInstruction>(scopeBoundsMarker);
                        break;
                    case ILInstruction instr:
                        yield return Either.Right<ScopeBoundsMarker, ILInstruction>(instr);
                        break;
                    case SequentialBlock seq:
                        for (int i = seq.Instructions.Count - 1; i >= 0; i--)
                        {
                            yield return Either.Right<ScopeBoundsMarker, ILInstruction>(seq.Instructions[i]);
                        }
                        break;
                    case WhileBlock whileLoop:
                        stack.Push(ScopeBoundsMarker.Start);
                        stack.Push(whileLoop.Condition);
                        foreach (var child in whileLoop.Children)
                        {
                            stack.Push(child);
                        }
                        stack.Push(ScopeBoundsMarker.End);
                        break;
                    case DoWhileBlock doWhileLoop:
                        stack.Push(ScopeBoundsMarker.Start);
                        foreach (var child in doWhileLoop.Children)
                        {
                            stack.Push(child);
                        }
                        stack.Push(doWhileLoop.Condition);
                        stack.Push(ScopeBoundsMarker.End);
                        break;
                    case ConditionalBlock cond:
                        stack.Push(cond.Condition);
                        stack.Push(ScopeBoundsMarker.Start);
                        foreach (var child in cond.TrueBranch)
                        {
                            stack.Push(child);
                        }
                        stack.Push(ScopeBoundsMarker.End);
                        stack.Push(ScopeBoundsMarker.Start);
                        foreach (var child in cond.FalseBranch)
                        {
                            stack.Push(child);
                        }
                        stack.Push(ScopeBoundsMarker.End);
                        break;
                    default: throw new InvalidOperationException("Unknown block type");
                }
            }
        }
    }
}
