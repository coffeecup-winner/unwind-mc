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
                            }
                            else if (types.ContainsKey(instr.Target))
                            {
                                types[instr.Source] = types[instr.Target];
                            }
                            else
                            {
                                throw new NotImplementedException();
                            }
                        }
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
                        }
                        if (instr.Source.Type == ILOperandType.Pointer)
                        {
                            types[operand].AddIndirectionLevel();
                        }
                        types.Remove(instr.Target);
                        break;
                    case ILInstructionType.Call:
                        if (!types.ContainsKey(instr.Target))
                        {
                            types[instr.Target] = new TypeBuilder();
                        }
                        types[instr.Target].AddFunctionTrait();
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
