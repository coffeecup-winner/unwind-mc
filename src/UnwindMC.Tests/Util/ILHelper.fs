module ILHelper

open System.Collections.Generic
open NDis86
open NUnit.Framework
open IL

let assertILEqual (expected: ILInstruction) (actual: ILInstruction): unit =
    let verified = new HashSet<ILInstruction>()
    let queue = new Queue<ILInstruction * ILInstruction>()
    queue.Enqueue((expected, actual))
    verified.Add(expected) |> ignore
    while queue.Count > 0 do
        let (expectedInstr, actualInstr) = queue.Dequeue()
        Assert.That(actualInstr.type_, Is.EqualTo(expectedInstr.type_));
        Assert.That(actualInstr.branch, Is.EqualTo(expectedInstr.branch));
        Assert.That(actualInstr.source, Is.EqualTo(expectedInstr.source));
        Assert.That(actualInstr.target, Is.EqualTo(expectedInstr.target));
        Assert.That(actualInstr.condition, Is.EqualTo(expectedInstr.condition));
        Assert.That(actualInstr.defaultChild.IsNone, Is.EqualTo(expectedInstr.defaultChild.IsNone));
        Assert.That(actualInstr.conditionalChild.IsNone, Is.EqualTo(expectedInstr.conditionalChild.IsNone));
        if expectedInstr.defaultChild.IsSome && verified.Add(expectedInstr.defaultChild.Value) then
            queue.Enqueue((expectedInstr.defaultChild.Value, actualInstr.defaultChild.Value))
        if expectedInstr.conditionalChild.IsSome && verified.Add(expectedInstr.conditionalChild.Value) then
            queue.Enqueue((expectedInstr.conditionalChild.Value, actualInstr.conditionalChild.Value))

let assertVarIds (instr: ILInstruction) (targetId: int) (sourceId: int): unit =
    Assert.That(instr.targetId, Is.EqualTo(targetId))
    Assert.That(instr.sourceId, Is.EqualTo(sourceId))

let SetOrder(instructions: ILInstruction[]): unit =
    for i in [0 .. instructions.Length - 1] do
        instructions.[i].order <- i

let Add (target: ILOperand) (source: ILOperand): ILInstruction =
    createBinaryInstruction ILInstructionType.Add target source

let Assign (target: ILOperand) (source: ILOperand): ILInstruction =
    createBinaryInstruction ILInstructionType.Assign target source

let Call (target: ILOperand): ILInstruction =
    createUnaryInstruction ILInstructionType.Call target

let Compare (target: ILOperand) (source: ILOperand): ILInstruction =
    createBinaryInstruction ILInstructionType.Compare target source

let Return (): ILInstruction =
    createBinaryInstruction Return NoOperand (Register OperandType.EAX)

let Subtract (target: ILOperand) (source: ILOperand): ILInstruction =
    createBinaryInstruction ILInstructionType.Subtract target source
