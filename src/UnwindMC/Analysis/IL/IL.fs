module IL

open NDis86

type ILOperand =
    | Value of int
    | Register of OperandType
    | Argument of int
    | Local of int
    | Pointer of OperandType * int

type BinaryInstruction<'op> = {
    left: 'op
    right: 'op
}

type UnaryInstruction<'op> = {
    operand: 'op
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
}

let binary (left: 'op) (right: 'op): BinaryInstruction<'op> = {
    left = left
    right = right
}

let branch (type_: BranchType) (target: uint64): BranchInstruction = {
    type_ = type_
    target = target
}
