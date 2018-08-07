extern crate regex;

extern crate unwind_mc;

mod analysis_helper;

#[test]
fn stage_test_find_max() {
    analysis_helper::analyze(
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
}
