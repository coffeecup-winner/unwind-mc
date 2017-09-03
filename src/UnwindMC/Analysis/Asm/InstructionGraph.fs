module rec InstructionGraph

open System
open System.Collections.Generic
open NDis86
open NLog
open IGraph
open TextWorkflow

[<Flags>]
type LinkType =
    | None = 0x00
    | Next = 0x01
    | Branch = 0x02
    | Call = 0x04
    | SwitchCaseJump = 0x08

type Link = {
    address: uint64
    targetAddress: uint64
    type_: LinkType
}

type ExtraData = {
    mutable functionAddress: uint64
    mutable importName: string
    mutable isProtected: bool
}

let logger = LogManager.GetCurrentClassLogger()

let disassemble (bytes: ArraySegment<byte>) (pc: uint64): T =
    logger.Info("Disassembling machine code")
    let disassembler = new Disassembler(DisassemblyMode.Mode32Bit, Nullable(DisassemblySyntax.Intel), pc)
    let instructions = disassembler.Disassemble(bytes.Array, bytes.Offset, bytes.Count, true, true)
    logger.Info("Done")
    let lastInstruction = instructions.[instructions.Count - 1]
    let instructionsDict = new SortedDictionary<uint64, Instruction>()
    for instr in instructions do
        instructionsDict.Add(instr.Offset, instr)
    new T(disassembler, bytes, pc, lastInstruction.Offset + (uint64)lastInstruction.Length,
        instructionsDict, new Dictionary<uint64, ExtraData>(), new Dictionary<uint64, List<Link>>(),
        new Dictionary<uint64, List<Link>>(), false, Func<Link, bool>(fun _ -> true))

type T (disassembler: Disassembler, bytes: ArraySegment<byte>, firstAddress: uint64, firstAddressAfterCode: uint64,
        instructions: SortedDictionary<uint64, Instruction>, extraData: Dictionary<uint64, ExtraData>, instructionLinks: Dictionary<uint64, List<Link>>,
        reverseLinks: Dictionary<uint64, List<Link>>, isReversed: bool, edgePredicate: Func<Link, bool>) =
    let disassembler: Disassembler = disassembler
    let bytes: ArraySegment<byte> = bytes
    let firstAddress: uint64 = firstAddress
    let firstAddressAfterCode: uint64 = firstAddressAfterCode
    let instructions: SortedDictionary<uint64, Instruction> = instructions
    let extraData: Dictionary<uint64, ExtraData> = extraData
    let instructionLinks: Dictionary<uint64, List<Link>> = instructionLinks
    let reverseLinks: Dictionary<uint64, List<Link>> = reverseLinks
    let isReversed: bool = isReversed
    let edgePredicate: Func<Link, bool> = edgePredicate

    interface IGraph<uint64, Instruction, Link> with
        member self.GetVertex(vertexId: uint64): Instruction =
            instructions.[vertexId]

        member self.Contains(address: uint64): bool =
            instructions.ContainsKey(address)

        member self.GetAdjacent(vertexId: uint64): IEnumerable<Either<uint64 * Link, string>> =
            seq {
                match (if isReversed then reverseLinks else instructionLinks).TryGetValue(vertexId) with
                | false, _ ->
                    yield Right("DFS: Couldn't find links for " + instructions.[vertexId].ToString())
                | true, links ->
                    for link in (links |> Seq.where (fun l -> edgePredicate.Invoke(l))) do
                        let linkAddress = if isReversed then link.address else link.targetAddress
                        if self.InBounds(linkAddress) then
                            yield Left((linkAddress, link))
                        else
                            yield Right("DFS: Jump outside of code section")
            }

        member self.GetSubgraph(subgraph: ISet<Instruction>): IGraph<uint64, Instruction, Link> =
            notSupported

        member self.WithEdgeFilter(predicate: Func<Link, bool>): IGraph<uint64, Instruction, Link> =
            new T(disassembler, bytes, firstAddress, firstAddressAfterCode, instructions,
                extraData, instructionLinks, reverseLinks, isReversed, predicate)
                :> IGraph<uint64, Instruction, Link>

        member self.ReverseEdges(): IGraph<uint64, Instruction, Link> =
            new T(disassembler, bytes, firstAddress, firstAddressAfterCode, instructions,
                extraData, instructionLinks, reverseLinks, not isReversed, edgePredicate)
                :> IGraph<uint64, Instruction, Link>

    member self.AsGenericGraph(): IGraph<uint64, Instruction, Link> =
        self :> IGraph<uint64, Instruction, Link>

    member self.Instructions: ICollection<Instruction> = instructions.Values :> ICollection<Instruction>
    member self.FirstAddressAfterCode: uint64 = firstAddressAfterCode

    member self.ContainsAddress(address: uint64): bool =
        instructions.ContainsKey(address)

    member self.Contains(instr: Instruction): bool =
        instructions.ContainsKey(instr.Offset)

    member self.InBounds(address: uint64): bool =
        address >= firstAddress && address < firstAddressAfterCode

    member self.GetNext(address: uint64): uint64 =
        [address + (uint64)1 .. firstAddressAfterCode - (uint64)1]
        |> Seq.find (fun a -> instructions.ContainsKey(a))

    member self.GetInValue(address: uint64): int =
        reverseLinks.[address].Count

    member self.GetOutValue(address: uint64): int =
        instructionLinks.[address].Count

    member self.GetBytes(address: uint64, size: int): ArraySegment<byte> =
        new ArraySegment<byte>(bytes.Array, self.ToByteArrayIndex(address), size)

    member self.GetExtraData(address: uint64): ExtraData =
        match extraData.TryGetValue(address) with
        | true, data -> data
        | false, _ ->
            let data = { functionAddress = 0uL; importName = null; isProtected = false }
            extraData.Add(address, data)
            data

    member self.AddLink(offset: uint64, targetOffset: uint64, type_: LinkType): unit =
        let link = { address = offset; targetAddress = targetOffset; type_ = type_ }

        let links =
            match instructionLinks.TryGetValue(offset) with
            | true, links -> links
            | false, _ ->
                let links = new List<Link>()
                instructionLinks.[offset] <- links
                links
        links.Add(link)

        let reverseLinks =
            match reverseLinks.TryGetValue(targetOffset) with
            | true, links -> links
            | false, _ ->
                let links = new List<Link>()
                reverseLinks.[targetOffset] <- links
                links
        reverseLinks.Add(link)

    member self.AddJumpTableEntry(address: uint64): bool =
        let data = self.ReadUInt32(address)
        if not (self.InBounds(data)) then
            false
        else
            self.MarkDataBytes(address, 4, System.String.Format("dd {0:x8}", data))
            true

    member self.AddJumpTableIndirectEntries(address: uint64, count: int): unit =
        let displayText =
            text {
                yield "db"
                let start = self.ToByteArrayIndex(address)
                for i in [0 .. count - 1] do
                    yield " %0x" %% bytes.Array.[start + i]
            } |> buildText plain
        self.MarkDataBytes(address, count, displayText)

    member self.MarkDataBytes(address: uint64, length: int, dataDisplayText: string): unit =
        // check if there is an instruction at address, otherwise split an existing one
        let mutable size =
            match instructions.TryGetValue(address) with
            | true, instr ->
                (int)instr.Length
            | false, _ ->
                let instr = self.SplitInstructionAt(address)
                (int)instr.Length - (int)(address - instr.Offset)
        // write a pseudo instruction
        let instrText =
            text {
                for i in [0 .. length - 1] do
                    yield "%02x" %% bytes.Array.[self.ToByteArrayIndex(address + (uint64)i)]
            } |> buildText plain
        instructions.[address] <- new Instruction(address, MnemonicCode.Inone, (byte)length, instrText, dataDisplayText,
            Array.empty, 0uy, OperandType.None, false, false, 0uy, 0uy, 0uy, 0uy, 0uy)
        self.GetExtraData(address).isProtected <- true
        // remove old instructions
        while size < length do
            let instr = instructions.[address + (uint64)size]
            instructions.Remove(address + (uint64)size) |> ignore
            size <- size + (int)instr.Length
        // if there are bytes left from the last removed instruction, re-disassemble them
        if size <> length then
            disassembler.SetPC(address + (uint64)length)
            let newInstructions = disassembler.Disassemble(bytes.Array, self.ToByteArrayIndex(address) + length, size - length, true, true)
            for newInstr in newInstructions do
                instructions.[newInstr.Offset] <- newInstr

    member self.SplitInstructionAt(address: uint64): Instruction =
        let mutable instrAddress = address - (uint64)1
        let oldInstr = ref null
        while not (instructions.TryGetValue(instrAddress, oldInstr)) do
            instrAddress <- instrAddress - (uint64)1
        // Split the old instruction and re-disassemble its first part
        let extraLength = (int)(address - instrAddress)
        disassembler.SetPC(instrAddress)
        let newInstructions = disassembler.Disassemble(bytes.Array, self.ToByteArrayIndex(instrAddress), extraLength, true, true)
        for newInstr in newInstructions do
            instructions.[newInstr.Offset] <- newInstr
        !oldInstr

    member self.ReadUInt32(address: uint64): uint64 =
        (uint64)(ArrayReader.readUInt32 bytes.Array (self.ToByteArrayIndex(address)))

    member self.Redisassemble(address: uint64): unit =
        // This function will fix any instructions that were incorrectly disassembled because of the data block that was treated as code
        if not (instructions.ContainsKey(address)) then
            self.SplitInstructionAt(address) |> ignore
        disassembler.SetPC(address)
        let maxDisassembleLength = 0x100
        let mutable disassembleLength = Math.Min(maxDisassembleLength, (int)(firstAddressAfterCode - address))
        for j in [0 .. disassembleLength] do
            match extraData.TryGetValue(address + (uint64)j) with
            | true, data when data.isProtected ->
                disassembleLength <- j
            | _ -> ()
        let newInstructions = disassembler.Disassemble(bytes.Array, self.ToByteArrayIndex(address), disassembleLength, true, true)
        let mutable fixedInstructions = false
        let rec run (list: Instruction list): unit =
            match list with
            | newInstr :: rest ->
                match instructions.TryGetValue(newInstr.Offset) with
                | true, oldInstr when oldInstr.Length = newInstr.Length ->
                    if not fixedInstructions then
                        run rest
                | _ ->
                    instructions.[newInstr.Offset] <- newInstr
                    for j in [1 .. (int)newInstr.Length - 1] do
                        if instructions.ContainsKey(newInstr.Offset + (uint64)j) then
                            instructions.Remove(newInstr.Offset + (uint64)j) |> ignore
                    fixedInstructions <- true
                    if rest.IsEmpty then
                        FIXME "extra disassemble size was too small"
                    run rest
            | [] -> ()
        // Take 1 instruction less since it can be incorrectly disassembled (partial data)
        run (newInstructions |> Seq.take (newInstructions.Count - 1) |> Seq.toList)

    member private self.ToByteArrayIndex(address: uint64): int =
        bytes.Offset + (int)(address - firstAddress)
