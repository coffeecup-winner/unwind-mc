using System;
using System.Collections.Generic;
using System.Linq;
using NDis86;
using UnwindMC.Analysis.Asm;
using UnwindMC.Collections;

namespace UnwindMC.Analysis.IL
{
    public class ILDecompiler
    {
        private readonly Dictionary<int, object> _stackObjects = new Dictionary<int, object>();
        private int _stackOffset;
        private int _framePointerOffset;
        private Instruction _prevInstr;

        private ILDecompiler()
        {
            _stackOffset = -4; // skip the return address on the stack
            _stackObjects.Add(_stackOffset, null);
        }

        public static ILInstruction Decompile(InstructionGraph graph, ulong address)
        {
            var ilConverter = new ILDecompiler();
            var instructions = new Dictionary<ulong, ILInstruction>();
            graph
                .WithEdgeFilter(e => (e.Type & InstructionGraph.LinkType.Next | InstructionGraph.LinkType.Branch | InstructionGraph.LinkType.SwitchCaseJump) != 0)
                .DFS(address, (instr, link) =>
            {
                var ilInstructions = ilConverter.Convert(instr);
                if (ilInstructions.Length > instr.Length)
                {
                    throw new NotSupportedException("TODO: not enough virtual addresses");
                }
                for (int i = 0; i < ilInstructions.Length; i++)
                {
                    instructions.Add(instr.Offset + (uint)i, ilInstructions[i]);
                }
                return true;
            });
            var il = new SortedList<ulong, ILInstruction>(instructions);
            var addresses = new Dictionary<ILInstruction, ulong>(instructions.Count);
            int order = 0;
            foreach (var pair in il)
            {
                addresses[pair.Value] = pair.Key;
                if (pair.Value.Type != ILInstructionType.None && pair.Value.Type != ILInstructionType.Virtual)
                {
                    pair.Value.SetOrder(order++);
                }
            }
            ILInstruction current = null;
            ILBranch lastBranch = null;
            bool isEmptyThen = false;
            foreach (var pair in il)
            {
                if (pair.Value.Type == ILInstructionType.None)
                {
                    continue;
                }
                if (current == null)
                {
                    current = pair.Value;
                    continue;
                }
                if (pair.Value.Type == ILInstructionType.Virtual)
                {
                    int index = il.IndexOfKey(pair.Value.Branch.Address);
                    while (il.Values[index].Type == ILInstructionType.None)
                    {
                        index++;
                    }
                    current.AddDefaultChild(il.Values[index]);
                    if (pair.Value.Branch.Type == ILBranchType.Next)
                    {
                        if (lastBranch != null)
                        {
                            isEmptyThen = true;
                        }
                        else
                        {
                            current = null;
                        }
                    }
                    else
                    {
                        lastBranch = pair.Value.Branch;
                    }
                    continue;
                }
                if (lastBranch != null)
                {
                    var branchType = isEmptyThen ? Complement(lastBranch.Type) : lastBranch.Type;
                    if (addresses[current.DefaultChild] > addresses[pair.Value])
                    {
                        current.AddConditionalChild(Complement(branchType), pair.Value);
                    }
                    else
                    {
                        current.AddConditionalChild(branchType, current.DefaultChild);
                        current.AddDefaultChild(pair.Value);
                    }
                    current = pair.Value;
                    lastBranch = null;
                    isEmptyThen = false;
                    continue;
                }
                current.AddDefaultChild(pair.Value);
                current = pair.Value;
            }
            return il.Values.First(i => i.Type != ILInstructionType.None && i.Type != ILInstructionType.Virtual);
        }

        private ILInstruction[] Convert(Instruction instr)
        {
            ILOperand[] operands;
            ILInstruction[] result;
            ILInstruction cond;
            ILInstruction branch;
            switch (instr.Code)
            {
                case MnemonicCode.Iadd:
                    operands = Convert(instr.Operands);
                    result = new[] { new ILInstruction(ILInstructionType.Add, operands[0], operands[1]) };
                    break;
                case MnemonicCode.Iand:
                    operands = Convert(instr.Operands);
                    result = new[] { new ILInstruction(ILInstructionType.And, operands[0], operands[1]) };
                    break;
                case MnemonicCode.Icall:
                    operands = Convert(instr.Operands);
                    result = new[] { new ILInstruction(ILInstructionType.Call, operands[0]) };
                    break;
                case MnemonicCode.Icdq:
                    result = new ILInstruction[0]; // Size extension seems to be irrelevant at the moment
                    break;
                case MnemonicCode.Icmovl:
                    operands = Convert(instr.Operands);
                    result = new[]
                    {
                        new ILInstruction(ILInstructionType.Virtual, branch: new ILBranch(ILBranchType.GreaterOrEqual, instr.Offset + instr.Length)),
                        new ILInstruction(ILInstructionType.Assign, operands[0], operands[1]),
                    };
                    break;
                case MnemonicCode.Icmp:
                    operands = Convert(instr.Operands);
                    result = new[] { new ILInstruction(ILInstructionType.Compare, operands[0], operands[1]) };
                    break;
                case MnemonicCode.Idec:
                    operands = Convert(instr.Operands);
                    result = new[] { new ILInstruction(ILInstructionType.Subtract, operands[0], ILOperand.FromValue(1)) };
                    break;
                case MnemonicCode.Iidiv:
                    operands = Convert(instr.Operands);
                    result = new[] { new ILInstruction(ILInstructionType.Divide, ILOperand.FromRegister(OperandType.EAX), operands[0]) };
                    break;
                case MnemonicCode.Iimul:
                    operands = Convert(instr.Operands);
                    result = new[] { new ILInstruction(ILInstructionType.Multiply, operands[0], operands[1]) };
                    break;
                case MnemonicCode.Ijae:
                    result = new[] { new ILInstruction(ILInstructionType.Virtual, branch: new ILBranch(ILBranchType.GreaterOrEqual, instr.GetTargetAddress())) };
                    break;
                case MnemonicCode.Ijmp:
                    result = new[] { new ILInstruction(ILInstructionType.Virtual, branch: new ILBranch(ILBranchType.Next, instr.GetTargetAddress())) };
                    break;
                case MnemonicCode.Ijz:
                    cond = GetVirtualConditionInstruction();
                    branch = new ILInstruction(ILInstructionType.Virtual, branch: new ILBranch(ILBranchType.Equal, instr.GetTargetAddress()));
                    result = cond == null ? new[] { branch } : new[] { cond, branch };
                    break;
                case MnemonicCode.Ijnz:
                    cond = GetVirtualConditionInstruction();
                    branch = new ILInstruction(ILInstructionType.Virtual, branch: new ILBranch(ILBranchType.NotEqual, instr.GetTargetAddress()));
                    result = cond == null ? new[] { branch } : new[] { cond, branch };
                    break;
                case MnemonicCode.Imov:
                    operands = Convert(instr.Operands);
                    if (operands[0].IsRegister(OperandType.EBP))
                    {
                        if (!operands[1].IsRegister(OperandType.ESP))
                        {
                            throw new NotSupportedException();
                        }
                        _framePointerOffset = _stackOffset;
                        return new ILInstruction[0];
                    }
                    if (operands[0].IsRegister(OperandType.ESP))
                    {
                        if (!operands[1].IsRegister(OperandType.EBP))
                        {
                            throw new NotSupportedException();
                        }
                        _stackOffset = _framePointerOffset;
                        return new ILInstruction[0];
                    }
                    result = new[] { new ILInstruction(ILInstructionType.Assign, operands[0], operands[1]) };
                    break;
                case MnemonicCode.Ineg:
                    operands = Convert(instr.Operands);
                    result = new[] { new ILInstruction(ILInstructionType.Negate, operands[0]) };
                    break;
                case MnemonicCode.Inot:
                    operands = Convert(instr.Operands);
                    result = new[] { new ILInstruction(ILInstructionType.Not, operands[0]) };
                    break;
                case MnemonicCode.Ior:
                    operands = Convert(instr.Operands);
                    result = new[] { new ILInstruction(ILInstructionType.Or, operands[0], operands[1]) };
                    break;
                case MnemonicCode.Ipush:
                    _stackOffset -= 4;
                    AddOrUpdateStackValue(_stackOffset);
                    result = new[] { new ILInstruction(ILInstructionType.None) };
                    break;
                case MnemonicCode.Ipop:
                    _stackOffset += 4;
                    result = new[] { new ILInstruction(ILInstructionType.None) };
                    break;
                case MnemonicCode.Iret:
                    _stackOffset += 4;
                    if (_stackOffset != 0)
                    {
                        throw new InvalidOperationException("Stack imbalance");
                    }
                    result = new[] { new ILInstruction(ILInstructionType.Return, source: ILOperand.FromRegister(OperandType.EAX)) };
                    break;
                case MnemonicCode.Ishl:
                    operands = Convert(instr.Operands);
                    result = new[] { new ILInstruction(ILInstructionType.ShiftLeft, operands[0], operands[1]) };
                    break;
                case MnemonicCode.Isar:
                    operands = Convert(instr.Operands);
                    result = new[] { new ILInstruction(ILInstructionType.ShiftRight, operands[0], operands[1]) };
                    break;
                case MnemonicCode.Isub:
                    operands = Convert(instr.Operands);
                    result = new[] { new ILInstruction(ILInstructionType.Subtract, operands[0], operands[1]) };
                    break;
                case MnemonicCode.Itest:
                    operands = Convert(instr.Operands);
                    if (operands[0].Type == ILOperandType.Register && operands[1].IsRegister(operands[0].Register))
                    {
                        result = new[] { new ILInstruction(ILInstructionType.Compare, operands[0], ILOperand.FromValue(0)) };
                        break;
                    }
                    else goto default;
                case MnemonicCode.Ixor:
                    operands = Convert(instr.Operands);
                    result = new[] { new ILInstruction(ILInstructionType.Xor, operands[0], operands[1]) };
                    break;
                default: throw new NotSupportedException($"Instruction `{instr}` is not supported yet");
            }
            _prevInstr = instr;
            return result;
        }

        private ILInstruction GetVirtualConditionInstruction()
        {
            if (_prevInstr.Code == MnemonicCode.Icmp || _prevInstr.Code == MnemonicCode.Itest)
            {
                return null;
            }
            switch (_prevInstr.Code)
            {
                case MnemonicCode.Idec:
                case MnemonicCode.Imov: // TODO: this line is a heuristic and might be wrong in some cases
                    var operands = Convert(_prevInstr.Operands);
                    return new ILInstruction(ILInstructionType.Compare, operands[0], ILOperand.FromValue(0));
                default: throw new NotSupportedException();
            }
        }

        private ILOperand[] Convert(IReadOnlyList<Operand> operands)
        {
            var result = new ILOperand[operands.Count];
            for (int i = 0; i < operands.Count; i++)
            {
                var op = Convert(operands[i]);
                if (op.Type == ILOperandType.Stack)
                {
                    AddOrUpdateStackValue(op.Offset);
                }
                result[i] = op;
            }
            return result;
        }

        private void AddOrUpdateStackValue(int offset)
        {
            if (!_stackObjects.TryGetValue(offset, out object value))
            {
                _stackObjects.Add(offset, null);
            }
        }

        private ILOperand Convert(Operand operand)
        {
            switch (operand.Type)
            {
                case OperandType.Register:
                    return ILOperand.FromRegister(operand.Base);
                case OperandType.Memory:
                    if (operand.Base == OperandType.ESP)
                    {
                        return ILOperand.FromStack(_stackOffset + (int)operand.GetMemoryOffset());
                    }
                    else if (operand.Base == OperandType.EBP)
                    {
                        return ILOperand.FromStack(_framePointerOffset + (int)operand.GetMemoryOffset());
                    }
                    else if (operand.Index == OperandType.None)
                    {
                        return ILOperand.FromPointer(operand.Base, (int)operand.GetMemoryOffset());
                    }
                    throw new NotSupportedException();
                case OperandType.Constant:
                case OperandType.Immediate:
                    return ILOperand.FromValue((int)operand.GetValue());
                default: throw new NotSupportedException();
            }
        }

        private static ILBranchType Complement(ILBranchType type)
        {
            switch (type)
            {
                case ILBranchType.Equal: return ILBranchType.NotEqual;
                case ILBranchType.NotEqual: return ILBranchType.Equal;
                case ILBranchType.Less: return ILBranchType.GreaterOrEqual;
                case ILBranchType.LessOrEqual: return ILBranchType.Greater;
                case ILBranchType.GreaterOrEqual: return ILBranchType.Less;
                case ILBranchType.Greater: return ILBranchType.LessOrEqual;
                default: throw new ArgumentException("Cannot find branch type complement");
            }
        }
    }
}
