using System;
using System.Collections.Generic;
using UnwindMC.Analysis.Flow;
using UnwindMC.Analysis.IL;

namespace UnwindMC.Analysis.Data
{
    public static class TypeResolver
    {
        public static IReadOnlyDictionary<ILOperand, Type> ResolveFunctionArguments(IReadOnlyList<IBlock> blocks)
        {
            var types = new Dictionary<ILOperand, TypeBuilder>();
            var currentIds = new Dictionary<ILOperand, int>();
            var nextId = 0;
            foreach (var instr in TraverseReversed(blocks))
            {
                switch (instr.Type)
                {
                    case ILInstructionType.Add:
                    case ILInstructionType.Compare:
                        if (instr.Source.Type == ILOperandType.Value)
                        {
                            if (!types.ContainsKey(instr.Target))
                            {
                                types[instr.Target] = new TypeBuilder();
                                currentIds[instr.Target] = nextId++;
                            }
                        }
                        else
                        {
                            if (types.ContainsKey(instr.Source) && types.ContainsKey(instr.Target))
                            {
                                if (types[instr.Source] != types[instr.Target])
                                {
                                    throw new InvalidOperationException("Type mismatch");
                                }
                            }
                            else if (types.ContainsKey(instr.Source))
                            {
                                types[instr.Target] = types[instr.Source];
                                currentIds[instr.Target] = nextId++;
                            }
                            else if (types.ContainsKey(instr.Target))
                            {
                                types[instr.Source] = types[instr.Target];
                                currentIds[instr.Source] = nextId++;
                            }
                            else
                            {
                                throw new NotImplementedException();
                            }
                        }
                        instr.SetVariableIds(GetCurrentId(currentIds, instr.Target), GetCurrentId(currentIds, instr.Source));
                        break;
                    case ILInstructionType.Assign:
                        if (!types.ContainsKey(instr.Target))
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
                        if (types.ContainsKey(operand))
                        {
                            var type = types[instr.Target].Build();
                            if (type.IsFunction)
                            {
                                types[operand].AddFunctionTrait();
                            }
                            types[operand].AddIndirectionLevel(type.IndirectionLevel);
                        }
                        else
                        {
                            types[operand] = types[instr.Target];
                            currentIds[operand] = nextId++;
                        }
                        if (instr.Source.Type == ILOperandType.Pointer)
                        {
                            types[operand].AddIndirectionLevel();
                        }
                        instr.SetVariableIds(GetCurrentId(currentIds, instr.Target), GetCurrentId(currentIds, operand));
                        types.Remove(instr.Target);
                        currentIds.Remove(instr.Target);
                        break;
                    case ILInstructionType.Call:
                        if (!types.ContainsKey(instr.Target))
                        {
                            types[instr.Target] = new TypeBuilder();
                            currentIds[instr.Target] = nextId++;
                        }
                        types[instr.Target].AddFunctionTrait();
                        instr.SetVariableIds(GetCurrentId(currentIds, instr.Target), GetCurrentId(currentIds, instr.Source));
                        break;
                }
            }
            var result = new Dictionary<ILOperand, Type>();
            foreach (var pair in types)
            {
                result[pair.Key] = pair.Value.Build();
            }
            return result;
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
                var loop = current as LoopBlock;
                if (loop != null)
                {
                    stack.Push(loop.Condition);
                    foreach (var child in loop.Children)
                    {
                        stack.Push(child);
                    }
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
