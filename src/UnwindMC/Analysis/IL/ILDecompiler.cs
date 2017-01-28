using NDis86;
using System;
using System.Collections.Generic;
using System.Linq;
using UnwindMC.Util;

namespace UnwindMC.Analysis.IL
{
    public class ILDecompiler
    {
        private readonly Dictionary<int, object> _stackObjects = new Dictionary<int, object>();
        private int _stackOffset;

        private ILDecompiler()
        {
            _stackOffset = -4; // skip the return address on the stack
            _stackObjects.Add(_stackOffset, null);
        }

        public static ILInstruction Decompile(InstructionGraph graph, ulong address)
        {
            var ilConverter = new ILDecompiler();
            var allowedLinks = InstructionGraph.LinkType.Next | InstructionGraph.LinkType.Branch | InstructionGraph.LinkType.SwitchCaseJump;
            var instructions = new Dictionary<ulong, ILInstruction>();
            graph.DFS(address, allowedLinks, (instr, link) =>
            {
                instructions.Add(instr.Offset, ilConverter.Convert(instr));
                return true;
            });
            var il = new SortedList<ulong, ILInstruction>(instructions);
            ILInstruction current = null;
            ILBranch lastBranch = null;
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
                    if (pair.Value.Branch.Type == ILBranchType.Next)
                    {
                        int index = il.IndexOfKey(pair.Value.Branch.Address);
                        while (il.Values[index].Type == ILInstructionType.None)
                        {
                            index++;
                        }
                        current.AddNext(il.Values[index]);
                        current = null;
                    }
                    else
                    {
                        lastBranch = pair.Value.Branch;
                        int index = il.IndexOfKey(pair.Value.Branch.Address);
                        while (il.Values[index].Type == ILInstructionType.None)
                        {
                            index++;
                        }
                        current.AddLeft(lastBranch.Type, il.Values[index]);
                    }
                    continue;
                }
                if (lastBranch != null)
                {
                    current.AddRight(lastBranch.Type, pair.Value);
                    current = pair.Value;
                    lastBranch = null;
                    continue;
                }
                current.AddNext(pair.Value);
                current = pair.Value;
            }
            return il.Values.First(i => i.Type != ILInstructionType.None && i.Type != ILInstructionType.Virtual);
        }

        private ILInstruction Convert(Instruction instr)
        {
            ILOperand[] operands;
            switch (instr.Code)
            {
                case MnemonicCode.Iadd:
                    operands = Convert(instr.Operands);
                    return new ILInstruction(ILInstructionType.Add, operands[0], operands[1]);
                case MnemonicCode.Icall:
                    operands = Convert(instr.Operands);
                    return new ILInstruction(ILInstructionType.Call, operands[0]);
                case MnemonicCode.Icmp:
                    operands = Convert(instr.Operands);
                    return new ILInstruction(ILInstructionType.Compare, operands[0], operands[1]);
                case MnemonicCode.Ijae:
                    return new ILInstruction(ILInstructionType.Virtual, branch: new ILBranch(ILBranchType.GreaterOrEqual, instr.GetTargetAddress()));
                case MnemonicCode.Ijmp:
                    return new ILInstruction(ILInstructionType.Virtual, branch: new ILBranch(ILBranchType.Next, instr.GetTargetAddress()));
                case MnemonicCode.Ijz:
                    return new ILInstruction(ILInstructionType.Virtual, branch: new ILBranch(ILBranchType.Equal, instr.GetTargetAddress()));
                case MnemonicCode.Imov:
                    operands = Convert(instr.Operands);
                    return new ILInstruction(ILInstructionType.Assign, operands[0], operands[1]);
                case MnemonicCode.Ipush:
                    _stackOffset -= 4;
                    AddOrUpdateStackValue(_stackOffset);
                    return new ILInstruction(ILInstructionType.None);
                case MnemonicCode.Ipop:
                    _stackOffset += 4;
                    return new ILInstruction(ILInstructionType.None);
                case MnemonicCode.Iret:
                    _stackOffset += 4;
                    if (_stackOffset != 0)
                    {
                        throw new InvalidOperationException("Stack imbalance");
                    }
                    return new ILInstruction(ILInstructionType.Return);
                case MnemonicCode.Itest:
                    operands = Convert(instr.Operands);
                    if (operands[0].Type == ILOperandType.Register && operands[0].Type == ILOperandType.Register && operands[0].Register == operands[1].Register)
                    {
                        return new ILInstruction(ILInstructionType.Compare, operands[0], ILOperand.FromValue(0));
                    }
                    else goto default;
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
            object value;
            if (!_stackObjects.TryGetValue(offset, out value))
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
                    else if (operand.Index == OperandType.None)
                    {
                        return ILOperand.FromPointer(operand.Base, (int)operand.GetMemoryOffset());
                    }
                    throw new NotSupportedException();
                case OperandType.Immediate:
                    return ILOperand.FromValue((int)operand.GetValue());
                default: throw new NotSupportedException();
            }
        }
    }
}
