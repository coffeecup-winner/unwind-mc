#![allow(dead_code)]

extern crate regex;

extern crate unwindmc;

mod analysis_helper;

#[test]
fn test_jump_target_address() {
    let analyzer = analysis_helper::analyze(
        "
        00000021: 75 f7              jnz 0x1a
        00000023: c3                 ret",
    );

    let (_, insn) = analyzer.graph().instructions_iter().next().unwrap();
    assert_eq!(insn.get_target_address(), 0x1a);
    assert_eq!(insn.assembly, "jnz 0x1a");
}

#[test]
fn test_jump_table_resolving() {
    analysis_helper::analyze(
        "
        004b154a: bb 03 00 00 00     mov ebx, 0x3
        004b154f: 3b c3              cmp eax, ebx
        004b1551: 77 0b              ja 0x4b155e
        004b1553: ff 24 85 5f 15 4b  jmp dword [eax*4+0x4b155f]
        00
        004b155a: 5f                 pop edi
        004b155b: 5e                 pop esi
        004b155c: 5b                 pop ebx
        004b155d: c9                 leave
        004b155e: c3                 ret
        004b155f: 5a 15 4b 00        004b155a (data)
        004b1563: 5b 15 4b 00        004b155b (data)
        004b1567: 5c 15 4b 00        004b155c (data)
        004b156b: 5d 15 4b 00        004b155d (data)
        004b156f: 55                 push ebp
        004b1570: 8b ec              mov ebp, esp
        004b1572: 8b 4d 0c           mov ecx, [ebp+0xc]
        004b1575: ff 49 04           dec dword [ecx+0x4]",
    );
}
