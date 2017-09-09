[<NUnit.Framework.TestFixture>]
module ILDecompilationTests

open System
open NDis86
open NUnit.Framework
open IL

[<Test>]
let testILWithFunctionPointers (): unit =
    let graph = AnalysisHelper.analyze """
        00400000: 56                 push esi
        00400001: 8b 74 24 08        mov esi, [esp+0x8]
        00400005: 3b 74 24 0c        cmp esi, [esp+0xc]
        00400009: 73 0d              jae 0x400018
        0040000b: 8b 06              mov eax, [esi]
        0040000d: 85 c0              test eax, eax
        0040000f: 74 02              jz 0x400013
        00400011: ff d0              call eax
        00400013: 83 c6 04           add esi, 0x4
        00400016: eb ed              jmp 0x400005
        00400018: 5e                 pop esi
        00400019: c3                 ret"""
    let il = ILDecompiler.decompile graph 0x400000uL

    let nop0 = Nop
    let asn0 = Assign <| binary (Register OperandType.ESI) (Stack 0)
    let cmp0 = Compare <| binary (Register OperandType.ESI) (Stack 4)
    let br0 = Branch <| branch GreaterOrEqual 10uL
    let asn1 = Assign <| binary (Register OperandType.EAX) (Pointer (OperandType.ESI, 0))
    let cmp1 = Compare <| binary (Register OperandType.EAX) (Value 0)
    let br1 = Branch <| branch Equal 8uL
    let call = Call <| unary (Register OperandType.EAX)
    let add = Add <| binary (Register OperandType.ESI) (Value 4)
    let br3 = Branch <| branch Unconditional 2uL
    let nop1 = Nop
    let ret = Return <| unary (Register OperandType.EAX)

    let expected = [| nop0; asn0; cmp0; br0; asn1; cmp1; br1; call; add; br3; nop1; ret |]

    Assert.That(il, Is.EqualTo(expected))

[<Test>]
let testILFindMax (): unit =
    let graph = AnalysisHelper.analyze """
        08048400: 56                 push esi
        08048401: 8b 4c 24 0c        mov ecx, [esp+0xc]
        08048405: b8 00 00 00 80     mov eax, 0x80000000
        0804840a: 85 c9              test ecx, ecx
        0804840c: 74 16              jz 0x8048424
        0804840e: 8b 54 24 08        mov edx, [esp+0x8]
        08048412: b8 00 00 00 80     mov eax, 0x80000000
        08048417: 8b 32              mov esi, [edx]
        08048419: 39 f0              cmp eax, esi
        0804841b: 0f 4c c6           cmovl eax, esi
        0804841e: 83 c2 04           add edx, 0x4
        08048421: 49                 dec ecx
        08048422: 75 f3              jnz 0x8048417
        08048424: 5e                 pop esi
        08048425: c3                 ret"""
    let il = ILDecompiler.decompile graph 0x8048400uL

    let nop0 = Nop
    let asn0 = Assign <| binary (Register OperandType.ECX) (Stack 4)
    let asn1 = Assign <| binary (Register OperandType.EAX) (Value Int32.MinValue)
    let cmp0 = Compare <| binary (Register OperandType.ECX) (Value 0)
    let br0 = Branch <| branch Equal 15uL
    let asn2 = Assign <| binary (Register OperandType.EDX) (Stack 0)
    let asn3 = Assign <| binary (Register OperandType.EAX) (Value Int32.MinValue)
    let asn4 = Assign <| binary (Register OperandType.ESI) (Pointer (OperandType.EDX, 0))
    let cmp1 = Compare <| binary (Register OperandType.EAX) (Register OperandType.ESI)
    let br1 = Branch <| branch GreaterOrEqual 11uL
    let asn5 = Assign <| binary (Register OperandType.EAX) (Register OperandType.ESI)
    let add0 = Add <| binary (Register OperandType.EDX) (Value 4)
    let sub0 = Subtract <| binary (Register OperandType.ECX) (Value 1)
    let cmp2 = Compare <| binary (Register OperandType.ECX) (Value 0)
    let br2 = Branch <| branch NotEqual 7uL
    let nop1 = Nop
    let ret = Return <| unary (Register OperandType.EAX)

    let expected = [| nop0; asn0; asn1; cmp0; br0; asn2; asn3; asn4; cmp1; br1; asn5; add0; sub0; cmp2; br2; nop1; ret |]

    Assert.That(il, Is.EqualTo(expected))
