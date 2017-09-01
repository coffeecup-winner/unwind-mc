module rec IL

open System
open System.Collections.Generic
open System.Text
open NDis86
open Common
open IGraph

type ILOperand
    = Value of int
    | Register of OperandType
    | Stack of int
    | Pointer of OperandType * int
    | NoOperand // TODO: replace by option on ILOperand
    with
        override self.ToString(): string =
            let sb = new StringBuilder()
            match self with
            | Value value ->
                sb.Append("Value: ")
                    .Append(value) |> ignore
            | Register reg ->
                sb.Append("Register: ")
                    .Append(Register) |> ignore
            | Stack offset ->
                sb.Append("Stack: ")
                    .Append(offset) |> ignore
            | Pointer (reg, offset) ->
                sb.Append("Pointer: ")
                    .Append("*(")
                    .Append(reg)
                    .Append(" + ")
                    .Append(offset)
                    .Append(")") |> ignore
            | NoOperand ->
                sb.Append("NoOperand") |> ignore
            sb.ToString()

let isRegister (operand: ILOperand) (reg: OperandType): bool =
    match operand with
    | Register r -> r = reg
    | _ -> false

type ILBranchType
    = Equal
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

type ILInstructionType
    = Nop
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
            let sb = new StringBuilder()
            let instr = self
            match instr.type_ with
            | Nop ->
                sb.Append("<nop>") |> ignore
            | Virtual ->
                sb.Append(instr.branch) |> ignore
            | And ->
                sb.Append(instr.target)
                    .Append(" & ")
                    .Append(instr.source) |> ignore
            | Add ->
                sb.Append(instr.target)
                    .Append(" += ")
                    .Append(instr.source) |> ignore
            | Assign ->
                sb.Append(instr.target)
                    .Append(" = ")
                    .Append(instr.source) |> ignore
            | Call ->
                sb.Append("Call ")
                    .Append(instr.target) |> ignore
            | Compare ->
                sb.Append(instr.target) |> ignore
                match instr.condition with
                | Equal -> sb.Append(" == ") |> ignore
                | NotEqual -> sb.Append(" != ") |> ignore
                | Less -> sb.Append(" < ") |> ignore
                | LessOrEqual -> sb.Append(" <= ") |> ignore
                | GreaterOrEqual -> sb.Append(" >= ") |> ignore
                | Greater -> sb.Append(" > ") |> ignore
                | _ -> raise (new InvalidOperationException("Invalid condition type"))
                sb.Append(instr.source) |> ignore
            | Divide ->
                sb.Append(instr.target)
                    .Append(" / ")
                    .Append(instr.source) |> ignore
            | Multiply ->
                sb.Append(instr.target)
                    .Append(" * ")
                    .Append(instr.source) |> ignore
            | Negate ->
                sb.Append("-")
                    .Append(instr.target) |> ignore
            | Not ->
                sb.Append("~")
                    .Append(instr.target) |> ignore
            | Or ->
                sb.Append(instr.target)
                    .Append(" | ")
                    .Append(instr.source) |> ignore
            | Return ->
                sb.Append("return") |> ignore
            | ShiftLeft ->
                sb.Append(instr.target)
                    .Append(" << ")
                    .Append(instr.source) |> ignore
            | ShiftRight ->
                sb.Append(instr.target)
                    .Append(" >> ")
                    .Append(instr.source) |> ignore
            | Subtract ->
                sb.Append(instr.target)
                    .Append(" -= ")
                    .Append(instr.source) |> ignore
            | Xor ->
                sb.Append(instr.target)
                    .Append(" ^ ")
                    .Append(instr.source) |> ignore
            sb.ToString()

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
            raise (new NotSupportedException())

let createILGraph: IGraph<ILInstruction, ILInstruction, obj> =
    new ILGraph(null, null) :> IGraph<ILInstruction, ILInstruction, obj>
