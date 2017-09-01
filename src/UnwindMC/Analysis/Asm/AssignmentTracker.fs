module AssignmentTracker

open NDis86
open NLog
open IGraph

let Logger = LogManager.GetCurrentClassLogger()

let rec find (graph: InstructionGraph.T) (address: uint64) (register: OperandType) (tryMatch: Instruction -> OperandType -> bool): LValue option =
    let mutable result = None
    let mutable skippedInitialInstruction = false
    (graph :> IGraph<uint64, Instruction, InstructionGraph.Link>)
        .WithEdgeFilter(fun e -> (e.type_ &&& InstructionGraph.LinkType.Next ||| InstructionGraph.LinkType.Branch ||| InstructionGraph.LinkType.SwitchCaseJump) <> InstructionGraph.LinkType.None)
        .ReverseEdges()
        .DFS(address, fun instr _ ->
            if not skippedInitialInstruction then
                skippedInitialInstruction <- true
                true
            else
                if tryMatch instr register then
                    if instr.Operands.[1].Type = OperandType.Register then
                        result <- find graph instr.Offset instr.Operands.[1].Base (fun i reg ->
                            i.Code = MnemonicCode.Imov && i.Operands.[0].Type = OperandType.Register && i.Operands.[0].Base = reg)
                    elif instr.Operands.[1].Type = OperandType.Immediate then
                        result <- Some instr.Operands.[1].LValue
                    else
                        Logger.Warn("Cannot track assignments from 0x{0:x8} operand type {1}", instr.Offset, instr.Operands.[1].Type)
                    false
                else
                    if graph.GetInValue(instr.Offset) > 1 then
                        Logger.Warn("Cannot track assignments from 0x{0:x8}, the number of incoming links > 1", instr.Offset)
                        false
                    else
                        match instr.Code with
                        | MnemonicCode.Imov ->
                            if instr.Operands.[0].Type = OperandType.Register && instr.Operands.[1].Type = OperandType.Register &&
                                instr.Operands.[0].Base = register then
                                result <- find graph instr.Offset instr.Operands.[1].Base tryMatch
                                false
                            else
                                true
                        | _ -> true
        ) |> ignore
    result
