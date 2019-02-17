extern crate libudis86_sys;
extern crate regex;

extern crate unwindmc;

mod analysis_helper;

use unwindmc::ast::*;
use unwindmc::ast_builder::*;
use unwindmc::cpp_emitter::*;
use unwindmc::decompiler;
use unwindmc::disassembler::Reg;
use unwindmc::flow_analyzer::*;
use unwindmc::il::*;
use unwindmc::il_decompiler;
use unwindmc::type_resolver::*;

#[test]
fn end_to_end_functions_array() {
    let project = analysis_helper::analyze(
        "
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
        00400019: c3                 ret",
    );

    let code = decompiler::decompile_function(project.graph(), 0x400000);
    let expected = "void sub_400000(void (**arg0)(), void (**arg1)())
        {
          void (**var0)();
          void (*var1)();
        
          var0 = arg0;
          while (var0 < arg1)
          {
            var1 = *(var0);
            if (var1 != 0)
            {
              var1();
            }
            var0 = var0 + 1;
          }
          return;
        }";
    assert_eq!(code, analysis_helper::strip_indent(expected));
}

#[test]
fn stage_test_functions_array() {
    let project = analysis_helper::analyze(
        "
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
        00400019: c3                 ret",
    );
    let il = il_decompiler::decompile(project.graph(), 0x400000)
        .expect("Failed to decompile IL");

    use unwindmc::il::BranchType::*;
    use unwindmc::il::ILBinaryOperator::*;
    use unwindmc::il::ILInstruction::*;
    use unwindmc::il::ILOperand::*;

    let expected = vec![
        Assign(binary(Register(Reg::ESI), Argument(0))),
        Branch(branch(
            GreaterOrEqual,
            Some(binary(Register(Reg::ESI), Argument(4))),
            7,
        )),
        Assign(binary(Register(Reg::EAX), Pointer(Reg::ESI, Reg::NONE, 0, 0))),
        Branch(branch(Equal, Some(binary(Register(Reg::EAX), Value(0))), 5)),
        Call(unary(Register(Reg::EAX))),
        Binary(Add, binary(Register(Reg::ESI), Value(4))),
        Branch(branch(Unconditional, None, 1)),
        Return(unary(Register(Reg::EAX))),
    ];

    assert_eq!(il, expected);

    let blocks = build_flow_graph(il);

    let expected = vec![
        Block::SequentialBlock(SequentialBlock {
            instructions: vec![Assign(binary(Register(Reg::ESI), Argument(0)))],
        }),
        Block::WhileBlock(WhileBlock {
            condition: invert_condition(vec![Branch(branch(
                GreaterOrEqual,
                Some(binary(Register(Reg::ESI), Argument(4))),
                7,
            ))]),
            body: vec![
                Block::SequentialBlock(SequentialBlock {
                    instructions: vec![Assign(binary(Register(Reg::EAX), Pointer(Reg::ESI, Reg::NONE, 0, 0)))],
                }),
                Block::ConditionalBlock(ConditionalBlock {
                    condition: invert_condition(vec![Branch(branch(
                        Equal,
                        Some(binary(Register(Reg::EAX), Value(0))),
                        5,
                    ))]),
                    true_branch: vec![Block::SequentialBlock(SequentialBlock {
                        instructions: vec![Call(unary(Register(Reg::EAX)))],
                    })],
                    false_branch: vec![],
                }),
                Block::SequentialBlock(SequentialBlock {
                    instructions: vec![Binary(Add, binary(Register(Reg::ESI), Value(4)))],
                }),
            ],
        }),
        Block::SequentialBlock(SequentialBlock {
            instructions: vec![Return(unary(Register(Reg::EAX)))],
        }),
    ];

    assert_eq!(blocks, expected);

    let (blocks, types) = TypeResolver::resolve_types(blocks);
    let parameter_types = &types.parameter_types;
    let variable_types = &types.variable_types;
    let local_types = &types.local_types;

    assert_eq!(parameter_types.len(), 2);
    assert_eq!(
        parameter_types[0],
        DataType::Pointer(Box::new(DataType::Function))
    );
    assert_eq!(
        parameter_types[1],
        DataType::Pointer(Box::new(DataType::Function))
    );

    assert_eq!(variable_types.len(), 2);
    assert_eq!(
        variable_types[0],
        DataType::Pointer(Box::new(DataType::Function))
    );
    assert_eq!(variable_types[1], DataType::Function);

    assert_eq!(local_types.len(), 0);

    let expected = vec![
        Block::SequentialBlock(SequentialBlock {
            instructions: vec![Assign(binary(
                (Register(Reg::ESI), Some(0)),
                (Argument(0), None),
            ))],
        }),
        Block::WhileBlock(WhileBlock {
            condition: invert_condition(vec![Branch(branch(
                GreaterOrEqual,
                Some(binary((Register(Reg::ESI), Some(0)), (Argument(4), None))),
                7,
            ))]),
            body: vec![
                Block::SequentialBlock(SequentialBlock {
                    instructions: vec![Assign(binary(
                        (Register(Reg::EAX), Some(1)),
                        (Pointer(Reg::ESI, Reg::NONE, 0, 0), Some(0)),
                    ))],
                }),
                Block::ConditionalBlock(ConditionalBlock {
                    condition: invert_condition(vec![Branch(branch(
                        Equal,
                        Some(binary((Register(Reg::EAX), Some(1)), (Value(0), None))),
                        5,
                    ))]),
                    true_branch: vec![Block::SequentialBlock(SequentialBlock {
                        instructions: vec![Call(unary((Register(Reg::EAX), Some(1))))],
                    })],
                    false_branch: vec![],
                }),
                Block::SequentialBlock(SequentialBlock {
                    instructions: vec![Binary(
                        Add,
                        binary((Register(Reg::ESI), Some(0)), (Value(4), None)),
                    )],
                }),
            ],
        }),
        Block::SequentialBlock(SequentialBlock {
            instructions: vec![Return(unary((Register(Reg::EAX), None)))],
        }),
    ];

    assert_eq!(blocks, expected);

    let func = AstBuilder::build_ast(String::from("function_array"), &blocks, &types);

    use unwindmc::ast::Expression::{Dereference, VarRef};
    use unwindmc::ast::Statement::{Assignment, FunctionCall, IfThenElse, While};
    let expected = vec![
        Assignment(
            Var::Var(String::from("var0")),
            VarRef(Var::Var(String::from("arg0"))),
        ),
        While(
            Expression::Binary(
                Operator::Less,
                Box::new(VarRef(Var::Var(String::from("var0")))),
                Box::new(VarRef(Var::Var(String::from("arg1")))),
            ),
            vec![
                Assignment(
                    Var::Var(String::from("var1")),
                    Dereference(Box::new(VarRef(Var::Var(String::from("var0"))))),
                ),
                IfThenElse(
                    Expression::Binary(
                        Operator::NotEqual,
                        Box::new(VarRef(Var::Var(String::from("var1")))),
                        Box::new(Expression::Value(0)),
                    ),
                    vec![FunctionCall(VarRef(Var::Var(String::from("var1"))))],
                    vec![],
                ),
                Assignment(
                    Var::Var(String::from("var0")),
                    Expression::Binary(
                        Operator::Add,
                        Box::new(VarRef(Var::Var(String::from("var0")))),
                        Box::new(Expression::Value(1)),
                    ),
                ),
            ],
        ),
        Statement::Return(None),
    ];

    assert_eq!(func.body, expected);

    let code = CppEmitter::emit(&func);
    let expected = "void function_array(void (**arg0)(), void (**arg1)())
        {
          void (**var0)();
          void (*var1)();
        
          var0 = arg0;
          while (var0 < arg1)
          {
            var1 = *(var0);
            if (var1 != 0)
            {
              var1();
            }
            var0 = var0 + 1;
          }
          return;
        }";
    assert_eq!(code, analysis_helper::strip_indent(expected));
}
