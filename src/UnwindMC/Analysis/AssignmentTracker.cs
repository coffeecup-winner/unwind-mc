using NDis86;
using NLog;
using System;
using UnwindMC.Collections;
using UnwindMC.Util;

namespace UnwindMC.Analysis
{
    public class AssignmentTracker
    {
        public static readonly Logger Logger = LogManager.GetCurrentClassLogger();

        private readonly InstructionGraph _graph;

        public AssignmentTracker(InstructionGraph graph)
        {
            _graph = graph;
        }

        public Option<LValue> Find(ulong address, OperandType register, Func<Instruction, OperandType, bool> tryMatch)
        {
            var result = Option<LValue>.None;
            bool skippedInitialInstruction = false;
            _graph.ReverseEdges().DFS(address, e => (e.Type & InstructionGraph.LinkType.Next | InstructionGraph.LinkType.Branch | InstructionGraph.LinkType.SwitchCaseJump) != 0, (instr, link) =>
            {
                if (!skippedInitialInstruction)
                {
                    skippedInitialInstruction = true;
                    return true;
                }

                if (tryMatch(instr, register))
                {
                    if (instr.Operands[1].Type == OperandType.Register)
                    {
                        result = Find(instr.Offset, instr.Operands[1].Base, (i, reg) =>
                            i.Code == MnemonicCode.Imov && i.Operands[0].Type == OperandType.Register && i.Operands[0].Base == reg);
                    }
                    else if (instr.Operands[1].Type == OperandType.Immediate)
                    {
                        result = Option.Some(instr.Operands[1].LValue);
                    }
                    else
                    {
                        Logger.Warn("Cannot track assignments from 0x{0:x8} operand type {1}", instr.Offset, instr.Operands[1].Type);
                    }
                    return false;
                }

                if (_graph.GetInValue(instr.Offset) > 1)
                {
                    Logger.Warn("Cannot track assignments from 0x{0:x8}, the number of incoming links > 1", instr.Offset);
                    return false;
                }

                switch (instr.Code)
                {
                    case MnemonicCode.Imov:
                        if (instr.Operands[0].Type == OperandType.Register && instr.Operands[1].Type == OperandType.Register &&
                            instr.Operands[0].Base == register)
                        {
                            result = Find(instr.Offset, instr.Operands[1].Base, tryMatch);
                            return false;
                        }
                        break;
                }
                return true;
            });
            return result;
        }
    }
}
