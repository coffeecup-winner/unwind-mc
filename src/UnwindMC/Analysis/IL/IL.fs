module IL

open NDis86

type ILOperand =
    | Value of int
    | Register of OperandType
    | Argument of int
    | Local of int
    | Pointer of OperandType * int
    | NoOperand // TODO: replace by option on ILOperand

type BinaryInstruction<'op> = {
    left: 'op
    right: 'op
    // TODO: remove this
    leftId: int
    rightId: int
}

type UnaryInstruction<'op> = {
    operand: 'op
    // TODO: remove this
    operandId: int
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

type ILBinaryOperator =
    | Add
    | And
    | Divide
    | Multiply
    | Or
    | ShiftLeft
    | ShiftRight
    | Subtract
    | Xor

type ILUnaryOperator =
    | Negate
    | Not

type ILInstruction<'op> =
    | Binary of ILBinaryOperator * BinaryInstruction<'op>
    | Unary of ILUnaryOperator * UnaryInstruction<'op>
    | Assign of BinaryInstruction<'op>
    | Branch of BranchInstruction
    | Call of UnaryInstruction<'op>
    | Compare of BinaryInstruction<'op>
    | Return of UnaryInstruction<'op> // TODO: remove data
    | Nop // TODO: remove this
    | Continue
    | Break

let unary (operand: 'op): UnaryInstruction<'op> = {
    operand = operand
    operandId = -1
}

let binary (left: 'op) (right: 'op): BinaryInstruction<'op> = {
    left = left
    leftId = -1
    right = right
    rightId = -1
}

let branch (type_: BranchType) (target: uint64): BranchInstruction = {
    type_ = type_
    target = target
}
