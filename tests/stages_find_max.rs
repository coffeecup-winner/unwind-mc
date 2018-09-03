extern crate libudis86_sys;
extern crate regex;

extern crate unwind_mc;

mod analysis_helper;

use libudis86_sys::ud_type::*;

use unwind_mc::flow_analyzer::*;
use unwind_mc::il::*;
use unwind_mc::il_decompiler;
use unwind_mc::type_resolver::*;

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

    let expected = vec![
        Nop,
        Assign(binary(Register(UD_R_ECX), Argument(4))),
        Assign(binary(Register(UD_R_EAX), Value(0x80000000))),
        Compare(binary(Register(UD_R_ECX), Value(0))),
        Branch(branch(Equal, 15)),
        Assign(binary(Register(UD_R_EDX), Argument(0))),
        Assign(binary(Register(UD_R_EAX), Value(0x80000000))),
        Assign(binary(Register(UD_R_ESI), Pointer(UD_R_EDX, 0))),
        Compare(binary(Register(UD_R_EAX), Register(UD_R_ESI))),
        Branch(branch(GreaterOrEqual, 11)),
        Assign(binary(Register(UD_R_EAX), Register(UD_R_ESI))),
        Binary(Add, binary(Register(UD_R_EDX), Value(4))),
        Binary(Subtract, binary(Register(UD_R_ECX), Value(1))),
        Compare(binary(Register(UD_R_ECX), Value(0))),
        Branch(branch(NotEqual, 7)),
        Nop,
        Return(unary(Register(UD_R_EAX))),
    ];

    assert_eq!(il, expected);

    let blocks = build_flow_graph(il);

    let expected = vec![
        Block::SequentialBlock(SequentialBlock {
            instructions: vec![
                Nop,
                Assign(binary(Register(UD_R_ECX), Argument(4))),
                Assign(binary(Register(UD_R_EAX), Value(0x80000000))),
            ],
        }),
        Block::ConditionalBlock(ConditionalBlock {
            condition: invert_condition(vec![
                Compare(binary(Register(UD_R_ECX), Value(0))),
                Branch(branch(Equal, 15)),
            ]),
            true_branch: vec![
                Block::SequentialBlock(SequentialBlock {
                    instructions: vec![
                        Assign(binary(Register(UD_R_EDX), Argument(0))),
                        Assign(binary(Register(UD_R_EAX), Value(0x80000000))),
                    ],
                }),
                Block::DoWhileBlock(DoWhileBlock {
                    condition: vec![
                        Compare(binary(Register(UD_R_ECX), Value(0))),
                        Branch(branch(NotEqual, 7)),
                    ],
                    body: vec![
                        Block::SequentialBlock(SequentialBlock {
                            instructions: vec![Assign(binary(
                                Register(UD_R_ESI),
                                Pointer(UD_R_EDX, 0),
                            ))],
                        }),
                        Block::ConditionalBlock(ConditionalBlock {
                            condition: invert_condition(vec![
                                Compare(binary(Register(UD_R_EAX), Register(UD_R_ESI))),
                                Branch(branch(GreaterOrEqual, 11)),
                            ]),
                            true_branch: vec![Block::SequentialBlock(SequentialBlock {
                                instructions: vec![Assign(binary(
                                    Register(UD_R_EAX),
                                    Register(UD_R_ESI),
                                ))],
                            })],
                            false_branch: vec![],
                        }),
                        Block::SequentialBlock(SequentialBlock {
                            instructions: vec![
                                Binary(Add, binary(Register(UD_R_EDX), Value(4))),
                                Binary(Subtract, binary(Register(UD_R_ECX), Value(1))),
                            ],
                        }),
                    ],
                }),
            ],
            false_branch: vec![],
        }),
        Block::SequentialBlock(SequentialBlock {
            instructions: vec![Nop, Return(unary(Register(UD_R_EAX)))],
        }),
    ];

    assert_eq!(blocks, expected);

    let (blocks, types) = TypeResolver::resolve_types(blocks);
    let parameter_types = types.parameter_types;
    let variable_types = types.variable_types;
    let local_types = types.local_types;

    assert_eq!(parameter_types.len(), 2);
    assert_eq!(
        parameter_types[0],
        DataType::Pointer(Box::new(DataType::Int32))
    );
    assert_eq!(parameter_types[1], DataType::Int32);

    assert_eq!(variable_types.len(), 5);
    assert_eq!(variable_types[0], DataType::Int32);
    assert_eq!(variable_types[1], DataType::Int32);
    assert_eq!(
        variable_types[2],
        DataType::Pointer(Box::new(DataType::Int32))
    );
    assert_eq!(variable_types[3], DataType::Int32);
    assert_eq!(variable_types[4], DataType::Int32);

    assert_eq!(local_types.len(), 0);

    let expected = vec![
        Block::SequentialBlock(SequentialBlock {
            instructions: vec![
                Nop,
                Assign(binary((Register(UD_R_ECX), Some(0)), (Argument(4), None))),
                Assign(binary(
                    (Register(UD_R_EAX), Some(1)),
                    (Value(0x80000000), None),
                )),
            ],
        }),
        Block::ConditionalBlock(ConditionalBlock {
            condition: invert_condition(vec![
                Compare(binary((Register(UD_R_ECX), Some(0)), (Value(0), None))),
                Branch(branch(Equal, 15)),
            ]),
            true_branch: vec![
                Block::SequentialBlock(SequentialBlock {
                    instructions: vec![
                        Assign(binary((Register(UD_R_EDX), Some(2)), (Argument(0), None))),
                        Assign(binary(
                            (Register(UD_R_EAX), Some(1)),
                            (Value(0x80000000), None),
                        )),
                    ],
                }),
                Block::DoWhileBlock(DoWhileBlock {
                    condition: vec![
                        Compare(binary((Register(UD_R_ECX), Some(0)), (Value(0), None))),
                        Branch(branch(NotEqual, 7)),
                    ],
                    body: vec![
                        Block::SequentialBlock(SequentialBlock {
                            instructions: vec![Assign(binary(
                                (Register(UD_R_ESI), Some(4)),
                                (Pointer(UD_R_EDX, 0), Some(2)),
                            ))],
                        }),
                        Block::ConditionalBlock(ConditionalBlock {
                            condition: invert_condition(vec![
                                Compare(binary(
                                    (Register(UD_R_EAX), Some(1)),
                                    (Register(UD_R_ESI), Some(4)),
                                )),
                                Branch(branch(GreaterOrEqual, 11)),
                            ]),
                            true_branch: vec![Block::SequentialBlock(SequentialBlock {
                                instructions: vec![Assign(binary(
                                    (Register(UD_R_EAX), Some(1)),
                                    (Register(UD_R_ESI), Some(4)),
                                ))],
                            })],
                            false_branch: vec![],
                        }),
                        Block::SequentialBlock(SequentialBlock {
                            instructions: vec![
                                Binary(
                                    Add,
                                    binary((Register(UD_R_EDX), Some(2)), (Value(4), None)),
                                ),
                                Binary(
                                    Subtract,
                                    binary((Register(UD_R_ECX), Some(0)), (Value(1), None)),
                                ),
                            ],
                        }),
                    ],
                }),
            ],
            false_branch: vec![],
        }),
        Block::SequentialBlock(SequentialBlock {
            instructions: vec![Nop, Return(unary((Register(UD_R_EAX), Some(1))))],
        }),
    ];

    assert_eq!(blocks, expected);
}
