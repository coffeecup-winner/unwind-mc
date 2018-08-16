extern crate libudis86_sys;
extern crate regex;

extern crate unwind_mc;

mod analysis_helper;

use libudis86_sys::ud_type::*;

use unwind_mc::il::*;
use unwind_mc::il_decompiler;

#[test]
fn stage_test_find_max() {
    let analyzer = analysis_helper::analyze(
        "
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
        08048425: c3                 ret",
    );
    let il = il_decompiler::decompile(analyzer.graph(), 0x8048400);

    use unwind_mc::il::BranchType::*;
    use unwind_mc::il::ILBinaryOperator::*;
    use unwind_mc::il::ILInstruction::*;
    use unwind_mc::il::ILOperand::*;

    let nop0 = Nop;
    let asn0 = Assign(binary(Register(UD_R_ECX), Argument(4)));
    let asn1 = Assign(binary(Register(UD_R_EAX), Value(0x80000000)));
    let cmp0 = Compare(binary(Register(UD_R_ECX), Value(0)));
    let br0 = Branch(branch(Equal, 15));
    let asn2 = Assign(binary(Register(UD_R_EDX), Argument(0)));
    let asn3 = Assign(binary(Register(UD_R_EAX), Value(0x80000000)));
    let asn4 = Assign(binary(Register(UD_R_ESI), Pointer(UD_R_EDX, 0)));
    let cmp1 = Compare(binary(Register(UD_R_EAX), Register(UD_R_ESI)));
    let br1 = Branch(branch(GreaterOrEqual, 11));
    let asn5 = Assign(binary(Register(UD_R_EAX), Register(UD_R_ESI)));
    let add0 = Binary(Add, binary(Register(UD_R_EDX), Value(4)));
    let sub0 = Binary(Subtract, binary(Register(UD_R_ECX), Value(1)));
    let cmp2 = Compare(binary(Register(UD_R_ECX), Value(0)));
    let br2 = Branch(branch(NotEqual, 7));
    let nop1 = Nop;
    let ret = Return(unary(Register(UD_R_EAX)));

    let expected = vec![
        nop0, asn0, asn1, cmp0, br0, asn2, asn3, asn4, cmp1, br1, asn5, add0, sub0, cmp2, br2,
        nop1, ret,
    ];

    assert_eq!(il, expected);
}
