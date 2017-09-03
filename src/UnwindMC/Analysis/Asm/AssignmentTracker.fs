module AssignmentTracker

open NDis86
open NLog
open Graphs

let logger = LogManager.GetCurrentClassLogger()

let rec find (graph: InstructionGraph.T) (address: uint64) (register: OperandType) (tryMatch: Instruction -> OperandType -> bool): LValue option =
    let mutable skippedInitialInstruction = false
    graph.AsGenericGraph()
        |> Graph.withEdgeFilter (fun e ->
            match e.type_ with
            | InstructionGraph.LinkType.Next
            | InstructionGraph.LinkType.Branch
            | InstructionGraph.LinkType.SwitchCaseJump ->
                true
            | _ ->
                false
        )
        |> Graph.reverseEdges
        |> Graph.dfsPick address (fun instr _ ->
            if not skippedInitialInstruction then
                skippedInitialInstruction <- true
                Continue
            else
                if tryMatch instr register then
                    if instr.Operands.[1].Type = OperandType.Register then
                        Return <| find graph instr.Offset instr.Operands.[1].Base (fun i reg ->
                            i.Code = MnemonicCode.Imov && i.Operands.[0].Type = OperandType.Register && i.Operands.[0].Base = reg)
                    elif instr.Operands.[1].Type = OperandType.Immediate then
                        Return <| Some instr.Operands.[1].LValue
                    else
                        logger.Warn("Cannot track assignments from 0x{0:x8} operand type {1}", instr.Offset, instr.Operands.[1].Type)
                        Return None
                else
                    if graph.GetInValue(instr.Offset) > 1 then
                        logger.Warn("Cannot track assignments from 0x{0:x8}, the number of incoming links > 1", instr.Offset)
                        Return None
                    else
                        match instr.Code with
                        | MnemonicCode.Imov ->
                            if instr.Operands.[0].Type = OperandType.Register && instr.Operands.[1].Type = OperandType.Register &&
                                instr.Operands.[0].Base = register then
                                Return <| find graph instr.Offset instr.Operands.[1].Base tryMatch
                            else
                                Continue
                        | _ -> Continue
        )
