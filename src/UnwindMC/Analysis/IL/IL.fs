module rec IL

open System
open System.Collections.Generic
open NDis86
open IGraph
open TextWorkflow

type ILOperand =
    | Value of int
    | Register of OperandType
    | Stack of int
    | Pointer of OperandType * int
    | NoOperand // TODO: replace by option on ILOperand

let isRegister (operand: ILOperand) (reg: OperandType): bool =
    match operand with
    | Register r -> r = reg
    | _ -> false

type ILBranchType =
    | Equal
    | NotEqual
    | Less
    | LessOrEqual
    | GreaterOrEqual
    | Greater
    | Next

type ILBranch = {
    type_: ILBranchType
    address: uint64
}

type ILInstructionType =
    | Nop
    | Virtual

    | Add
    | And
    | Assign
    | Call
    | Compare
    | Divide
    | Multiply
    | Negate
    | Not
    | Or
    | Return
    | ShiftLeft
    | ShiftRight
    | Subtract
    | Xor

[<CustomEquality; NoComparison>]
type ILInstruction = {
    type_: ILInstructionType
    target: ILOperand
    mutable targetId: int
    source: ILOperand
    mutable sourceId: int
    branch: ILBranch
    mutable defaultChild: ILInstruction option
    mutable condition: ILBranchType
    mutable conditionalChild: ILInstruction option
    mutable order: int
}   with
        // TODO: stop using this type as hash key and remove Equals/GetHashCode overrides
        override self.Equals(other: obj): bool =
            match other with
            | :? ILInstruction as instr -> self.order = instr.order
            | _ -> false

        override self.GetHashCode(): int =
            let mutable hash = 17
            hash <- 37 * self.order
            hash

        override self.ToString(): string =
            text {
                let instr = self
                match instr.type_ with
                | Nop ->
                    yield "<nop>"
                | Virtual ->
                    yield "%A" %% instr.branch
                | And ->
                    yield "%A" %% instr.target
                    yield " & "
                    yield "%A" %% instr.source
                | Add ->
                    yield "%A" %% instr.target
                    yield " += "
                    yield "%A" %% instr.source
                | Assign ->
                    yield "%A" %% instr.target
                    yield " = "
                    yield "%A" %% instr.source
                | Call ->
                    yield "Call "
                    yield "%A" %% instr.target
                | Compare ->
                    yield "%A" %% instr.target
                    yield
                        match instr.condition with
                        | Equal -> " == "
                        | NotEqual -> " != "
                        | Less -> " < "
                        | LessOrEqual -> " <= "
                        | GreaterOrEqual -> " >= "
                        | Greater -> " > "
                        | _ -> failwith "Invalid condition type"
                    yield "%A" %% instr.source
                | Divide ->
                    yield "%A" %% instr.target
                    yield " / "
                    yield "%A" %% instr.source
                | Multiply ->
                    yield "%A" %% instr.target
                    yield " * "
                    yield "%A" %% instr.source
                | Negate ->
                    yield "-"
                    yield "%A" %% instr.target
                | Not ->
                    yield "~"
                    yield "%A" %% instr.target
                | Or ->
                    yield "%A" %% instr.target
                    yield " | "
                    yield "%A" %% instr.source
                | Return ->
                    yield "return"
                | ShiftLeft ->
                    yield "%A" %% instr.target
                    yield " << "
                    yield "%A" %% instr.source
                | ShiftRight ->
                    yield "%A" %% instr.target
                    yield " >> "
                    yield "%A" %% instr.source
                | Subtract ->
                    yield "%A" %% instr.target
                    yield " -= "
                    yield "%A" %% instr.source
                | Xor ->
                    yield "%A" %% instr.target
                    yield " ^ "
                    yield "%A" %% instr.source
            } |> buildText plain

let createNullaryInstruction (type_: ILInstructionType): ILInstruction = {
    type_ = type_
    target = NoOperand
    targetId = -1
    source = NoOperand
    sourceId = -1
    branch = { type_ = ILBranchType.Equal; address = 0uL } // ???
    defaultChild = None
    condition = ILBranchType.Equal // ???
    conditionalChild = None
    order = 0
}

let createUnaryInstruction (type_: ILInstructionType) (target: ILOperand): ILInstruction = {
    type_ = type_
    target = target
    targetId = -1
    source = NoOperand
    sourceId = -1
    branch = { type_ = ILBranchType.Equal; address = 0uL } // ???
    defaultChild = FSharp.Core.Option.None
    condition = ILBranchType.Equal // ???
    conditionalChild = FSharp.Core.Option.None
    order = 0
}

let createBinaryInstruction (type_: ILInstructionType) (target: ILOperand) (source: ILOperand): ILInstruction = {
    type_ = type_
    target = target
    targetId = -1
    source = source
    sourceId = -1
    branch = { type_ = ILBranchType.Equal; address = 0uL } // ???
    defaultChild = None
    condition = ILBranchType.Equal // ???
    conditionalChild = None
    order = 0
}

let createBranchInstruction (branchType: ILBranchType) (address: uint64): ILInstruction = {
    type_ = Virtual
    target = NoOperand
    targetId = -1
    source = NoOperand
    sourceId = -1
    branch = { type_ = branchType; address = address }
    defaultChild = None
    condition = ILBranchType.Equal // ???
    conditionalChild = None
    order = 0
}

type ILGraph (subgraph: ISet<ILInstruction>, edgePredicate: Func<obj, bool>) =
    let _subgraph: ISet<ILInstruction> = subgraph
    let _edgePredicate: Func<obj, bool> = edgePredicate

    let contains (vertex: ILInstruction): bool =
        _subgraph = null || _subgraph.Contains(vertex)

    interface IGraph<ILInstruction, ILInstruction, obj> with
        member self.Contains (vertex: ILInstruction): bool =
            contains vertex

        member self.GetVertex (vertexId: ILInstruction): ILInstruction =
            vertexId

        member self.GetAdjacent (vertex: ILInstruction): IEnumerable<Either<ILInstruction * obj, string>> =
            seq {
                match vertex.conditionalChild with
                | Some child when child.order > vertex.order && contains child -> yield Left (child, null)
                | _ -> ()
                match vertex.defaultChild with
                | Some child when child.order > vertex.order && contains child -> yield Left(child, null)
                | _ -> ()
            }

        member self.GetSubgraph (subgraph: ISet<ILInstruction>): IGraph<ILInstruction, ILInstruction, obj> =
            new ILGraph(subgraph, _edgePredicate) :> IGraph<ILInstruction, ILInstruction, obj>

        member self.WithEdgeFilter (predicate: Func<obj, bool>): IGraph<ILInstruction, ILInstruction, obj> =
            new ILGraph(_subgraph, predicate) :> IGraph<ILInstruction, ILInstruction, obj>

        member self.ReverseEdges(): IGraph<ILInstruction, ILInstruction, obj> =
            notSupported

let createILGraph: IGraph<ILInstruction, ILInstruction, obj> =
    new ILGraph(null, null) :> IGraph<ILInstruction, ILInstruction, obj>
