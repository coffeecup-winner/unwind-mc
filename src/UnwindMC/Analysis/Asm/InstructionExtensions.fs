module InstructionExtensions

open NDis86

type Operand with
    member self.GetValue (): int64 =
        match (int)self.Size with
        | 8 -> (int64)self.LValue.sbyte
        | 16 -> (int64)self.LValue.sword
        | 32 -> (int64)self.LValue.sdword
        | 64 -> self.LValue.sqword
        | _ -> impossible

    member self.GetMemoryOffset (): int64 =
        match (int)self.Offset with
        | 0 -> 0L
        | 8 -> (int64)self.LValue.sbyte
        | 16 -> (int64)self.LValue.sword
        | 32 -> (int64)self.LValue.sdword
        | 64 -> self.LValue.sqword
        | _ -> impossible

type Instruction with
    member self.GetTargetAddress (): uint64 =
        let targetOffset = self.Offset + (uint64)self.Length
        (uint64)((int64)targetOffset + self.Operands.[0].GetValue())
