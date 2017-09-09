module IL

open NDis86

type ILOperand =
    | Value of int
    | Register of OperandType
    | Stack of int
    | Pointer of OperandType * int
    | NoOperand // TODO: replace by option on ILOperand

type BinaryInstruction = {
    left: ILOperand
    right: ILOperand
    // TODO: remove this
    mutable leftId: int
    mutable rightId: int
}

type UnaryInstruction = {
    operand: ILOperand
    // TODO: remove this
    mutable operandId: int
}

type BranchType =
    | Equal
    | NotEqual
    | Less
    | LessOrEqual
    | GreaterOrEqual
    | Greater
    | Unconditional

type BranchInstruction = {
    type_: BranchType
    target: uint64
}

type ILInstruction =
    | Add of BinaryInstruction
    | And of BinaryInstruction
    | Assign of BinaryInstruction
    | Branch of BranchInstruction
    | Call of UnaryInstruction
    | Compare of BinaryInstruction
    | Divide of BinaryInstruction
    | Multiply of BinaryInstruction
    | Negate of UnaryInstruction
    | Not of UnaryInstruction
    | Or of BinaryInstruction
    | Return of UnaryInstruction // TODO: remove data
    | ShiftLeft of BinaryInstruction
    | ShiftRight of BinaryInstruction
    | Subtract of BinaryInstruction
    | Xor of BinaryInstruction
    | Nop // TODO: remove this

let unary (operand: ILOperand): UnaryInstruction = {
    operand = operand
    operandId = -1
}

let binary (left: ILOperand) (right: ILOperand): BinaryInstruction = {
    left = left
    leftId = -1
    right = right
    rightId = -1
}

let branch (type_: BranchType) (target: uint64): BranchInstruction = {
    type_ = type_
    target = target
}
