module AffectedFlagsTable

open System
open System.Linq
open NDis86

[<Flags>]
type FlagsRegister =
    | Carry = 0x001
    | Parity = 0x004
    | Adjust = 0x010
    | Zero = 0x040
    | Sign = 0x080
    | Trap = 0x100
    | InterruptEnable = 0x200
    | Direction = 0x400
    | Overflow = 0x800

let private Table: FlagsRegister[] = Array.zeroCreate (Enum.GetValues(typeof<MnemonicCode>).Length)

let get (code: MnemonicCode): FlagsRegister = Table.[(int)code]

let allFlags: FlagsRegister[] = Enum.GetValues(typeof<FlagsRegister>).Cast<FlagsRegister>().ToArray()

do
    let none: FlagsRegister = enum<FlagsRegister>(0)
    let all: FlagsRegister = FlagsRegister.Overflow ||| FlagsRegister.Sign ||| FlagsRegister.Zero ||| FlagsRegister.Adjust ||| FlagsRegister.Carry ||| FlagsRegister.Parity

    Table.[(int)MnemonicCode.Iadd] <- all
    Table.[(int)MnemonicCode.Iand] <- all
    Table.[(int)MnemonicCode.Icall] <- none
    Table.[(int)MnemonicCode.Icdq] <- none
    Table.[(int)MnemonicCode.Icmovl] <- none
    Table.[(int)MnemonicCode.Icmp] <- all
    Table.[(int)MnemonicCode.Idec] <- all &&& ~~~FlagsRegister.Carry
    Table.[(int)MnemonicCode.Iidiv] <- all
    Table.[(int)MnemonicCode.Iimul] <- all
    Table.[(int)MnemonicCode.Ijae] <- none
    Table.[(int)MnemonicCode.Ijmp] <- none
    Table.[(int)MnemonicCode.Ijz] <- none
    Table.[(int)MnemonicCode.Ijnz] <- none
    Table.[(int)MnemonicCode.Imov] <- none
    Table.[(int)MnemonicCode.Ineg] <- all
    Table.[(int)MnemonicCode.Inot] <- none
    Table.[(int)MnemonicCode.Ior] <- all
    Table.[(int)MnemonicCode.Ipush] <- none
    Table.[(int)MnemonicCode.Ipop] <- none
    Table.[(int)MnemonicCode.Iret] <- none
    Table.[(int)MnemonicCode.Isar] <- all
    Table.[(int)MnemonicCode.Ishl] <- all
    Table.[(int)MnemonicCode.Isub] <- all
    Table.[(int)MnemonicCode.Itest] <- all
    Table.[(int)MnemonicCode.Ixor] <- all
