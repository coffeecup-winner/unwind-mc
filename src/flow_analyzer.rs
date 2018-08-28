use either::*;

use il::*;

#[derive(Debug, PartialEq, Eq)]
pub struct ConditionalBlock<Op> {
    pub condition: Vec<ILInstruction<Op>>,
    pub true_branch: Vec<Block<Op>>,
    pub false_branch: Vec<Block<Op>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct DoWhileBlock<Op> {
    pub condition: Vec<ILInstruction<Op>>,
    pub body: Vec<Block<Op>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ForBlock<Op> {
    pub condition: Vec<ILInstruction<Op>>,
    pub modifier: Vec<ILInstruction<Op>>,
    pub body: Vec<Block<Op>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SequentialBlock<Op> {
    pub instructions: Vec<ILInstruction<Op>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct WhileBlock<Op> {
    pub condition: Vec<ILInstruction<Op>>,
    pub body: Vec<Block<Op>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Block<Op> {
    ConditionalBlock(ConditionalBlock<Op>),
    DoWhileBlock(DoWhileBlock<Op>),
    ForBlock(ForBlock<Op>),
    SequentialBlock(SequentialBlock<Op>),
    WhileBlock(WhileBlock<Op>),
}

struct LoopBounds {
    pub condition: usize,
    pub past_loop: usize,
}

pub fn build_flow_graph(il: Vec<ILInstruction<ILOperand>>) -> Vec<Block<ILOperand>> {
    let mut branch_targets: Vec<Option<usize>> = (0..il.len()).map(|_| None).collect();
    for pair in il.iter().enumerate() {
        match pair {
            (address, ILInstruction::Branch(br)) => {
                branch_targets[br.target as usize] = Some(address)
            }
            _ => {}
        }
    }
    let list: Vec<(usize, ILInstruction<ILOperand>, Option<usize>)> = il
        .into_iter()
        .zip(branch_targets)
        .enumerate()
        .map(|(a, (b, c))| (a, b, c))
        .collect();

    scope(
        &LoopBounds {
            condition: (-1 as i32) as usize,
            past_loop: (-1 as i32) as usize,
        },
        &list[..],
    )
}

fn scope(
    loop_bounds: &LoopBounds,
    list: &[(usize, ILInstruction<ILOperand>, Option<usize>)],
) -> Vec<Block<ILOperand>> {
    let mut seq = vec![];
    sequence(loop_bounds, list, &mut seq);
    let mut result = vec![];
    let mut insns = vec![];
    for elem in seq {
        if let Left(insn) = elem {
            insns.push(insn);
            continue;
        }
        if insns.len() > 0 {
            result.push(Block::SequentialBlock(SequentialBlock {
                instructions: insns,
            }));
            insns = vec![];
        }
        if let Right(block) = elem {
            result.push(block);
        } else {
            panic!("impossible");
        }
    }
    if insns.len() > 0 {
        result.push(Block::SequentialBlock(SequentialBlock {
            instructions: insns,
        }));
    }
    result
}

fn sequence(
    loop_bounds: &LoopBounds,
    list: &[(usize, ILInstruction<ILOperand>, Option<usize>)],
    result: &mut Vec<Either<ILInstruction<ILOperand>, Block<ILOperand>>>,
) {
    use il::ILInstruction::*;
    match list {
        /* for loop *
           ...
           jmp COND
           START:
           <modifier>
           COND:
           <neg-cond>
           br END
           <body>
           jmp START
           END:
           ...
         */
        [(start, Branch(br), _), (_, _, Some(end)), rest..]
            if br.type_ == BranchType::Unconditional =>
        {
            let loop_ = LoopBounds {
                condition: start + 1,
                past_loop: end + 1,
            };
            let (body, rest) = rest.split_at(end - start - 1);
            let block = for_loop(&loop_, br.target as usize - start, body);
            let (_, rest) = rest.split_at(1);
            result.push(Right(block));
            sequence(loop_bounds, rest, result);
        }
        /* while loop *
           ...
           START:
           <neg-cond>
           br END
           <body>
           jmp START
           END:
           ...
         */
        [(start, Compare(_), Some(end)), (_, Branch(br), _), _..] => {
            assert_eq!(end + 1, br.target as usize);
            let (_, test) = list.split_at(end - start);
            assert!(match test[0] {
                (_, Branch(ref br), _) if br.type_ == BranchType::Unconditional => true,
                _ => false,
            });
            // TODO assert_eq!()
            let loop_ = LoopBounds {
                condition: *start,
                past_loop: br.target as usize,
            };
            let (body, rest) = list.split_at(end - start);
            let (_, rest) = rest.split_at(1);
            let block = while_loop(&loop_, body);
            result.push(Right(block));
            sequence(loop_bounds, rest, result);
        }
        /* do while loop *
           ...
           START:
           <body>
           <condition>
           br START
           ...
         */
        [(start, _, Some(end)), _..] if end > start => {
            let loop_ = LoopBounds {
                condition: end - 1,
                past_loop: end + 1,
            };
            let (body, rest) = list.split_at(loop_.past_loop - start);
            let block = do_while_loop(&loop_, loop_.past_loop - start - 2, body);
            result.push(Right(block));
            sequence(loop_bounds, rest, result);
        }
        /* conditional *
         * if-then-else *  |  * if then *   |  * if else *
           ...             |    ...         |    ...
           <neg-cond>      |    <neg-cond>  |    <neg-cond>
           br FALSE        |    br END      |    br FALSE
           <true>          |    <true>      |    jmp END
           jmp END         |    END:        |    FALSE:
           FALSE:          |    ...         |    <false>
           <false>                          |    END:
           END:                             |    ...
           ...
         */
        [(start, Compare(_), _), (_, Branch(false_branch), _), _..]
            if false_branch.type_ != BranchType::Unconditional =>
        {
            let false_branch = false_branch.target as usize;
            let (_, false_branch_test) = list.split_at(false_branch - start - 1);
            let end = match false_branch_test {
                [(_, Branch(br), _), _..]
                    if br.type_ == BranchType::Unconditional
                        && br.target != loop_bounds.condition as u64
                        && br.target != loop_bounds.past_loop as u64 =>
                {
                    br.target as usize
                }
                _ => false_branch, // no else case
            };
            let (body, rest) = list.split_at(end - start);
            let block = conditional(loop_bounds, false_branch - start, body);
            result.push(Right(block));
            sequence(loop_bounds, rest, result);
        }
        /* loop control *
         * continue *     |  * continue *   |  * break *
           ...            |  ...            |  ...
           LOOP_COND:     |  jmp LOOP_COND  |  jmp LOOP_END
           ...            |  ...            |  ...
           jmp LOOP_COND  |  LOOP_COND:     |  LOOP_END:
           ...            |  ...            |  ...
         */
        [(_, Branch(br), _), rest..]
            if br.type_ == BranchType::Unconditional
                && br.target == loop_bounds.condition as u64 =>
        {
            result.push(Left(Continue));
            sequence(loop_bounds, rest, result);
        }
        [(_, Branch(br), _), rest..]
            if br.type_ == BranchType::Unconditional
                && br.target == loop_bounds.past_loop as u64 =>
        {
            result.push(Left(Break));
            sequence(loop_bounds, rest, result);
        }
        /* sequential flow *
           ...
         */
        [(_, insn, _), rest..] => {
            result.push(Left(insn.clone()));
            sequence(loop_bounds, rest, result);
        }
        /* end of scope *
         */
        [] => {}
    }
}

fn conditional(
    loop_bounds: &LoopBounds,
    false_branch_idx: usize,
    list: &[(usize, ILInstruction<ILOperand>, Option<usize>)],
) -> Block<ILOperand> {
    if false_branch_idx == list.len() {
        // no else case
        let (condition, list) = list.split_at(2);
        Block::ConditionalBlock(ConditionalBlock {
            condition: invert_condition(get_instructions(condition)),
            true_branch: scope(loop_bounds, list),
            false_branch: vec![],
        })
    } else {
        let (condition, list) = list.split_at(2);
        let (true_branch, list) = list.split_at(false_branch_idx - 3);
        let (_, list) = list.split_at(1);
        Block::ConditionalBlock(ConditionalBlock {
            condition: invert_condition(get_instructions(condition)),
            true_branch: scope(loop_bounds, true_branch),
            false_branch: scope(loop_bounds, list),
        })
    }
}

fn for_loop(
    loop_bounds: &LoopBounds,
    condition_idx: usize,
    list: &[(usize, ILInstruction<ILOperand>, Option<usize>)],
) -> Block<ILOperand> {
    let (modifier, list) = list.split_at(condition_idx - 1);
    let body_start = list
        .iter()
        .position(|(_, insn, _)| match insn {
            ILInstruction::Branch(br)
                if br.type_ != BranchType::Unconditional
                    && br.target == loop_bounds.past_loop as u64 =>
            {
                true
            }
            _ => false,
        }).unwrap();
    let (condition, list) = list.split_at(body_start + 1);
    Block::ForBlock(ForBlock {
        condition: invert_condition(get_instructions(condition)),
        modifier: get_instructions(modifier),
        body: scope(loop_bounds, list),
    })
}

fn do_while_loop(
    loop_bounds: &LoopBounds,
    condition_idx: usize,
    list: &[(usize, ILInstruction<ILOperand>, Option<usize>)],
) -> Block<ILOperand> {
    let (list, condition) = list.split_at(condition_idx);
    let mut list = list.to_vec();
    list[0].2 = None;
    Block::DoWhileBlock(DoWhileBlock {
        condition: get_instructions(condition),
        body: scope(loop_bounds, &list[..]),
    })
}

fn while_loop(
    loop_bounds: &LoopBounds,
    list: &[(usize, ILInstruction<ILOperand>, Option<usize>)],
) -> Block<ILOperand> {
    let (condition, list) = list.split_at(2);
    let mut list = list.to_vec();
    list[0].2 = None;
    Block::WhileBlock(WhileBlock {
        condition: invert_condition(get_instructions(condition)),
        body: scope(loop_bounds, &list[..]),
    })
}

fn get_instructions(
    list: &[(usize, ILInstruction<ILOperand>, Option<usize>)],
) -> Vec<ILInstruction<ILOperand>> {
    list.to_vec().into_iter().map(|(_, i, _)| i).collect()
}

pub fn invert_condition<Op>(mut condition: Vec<ILInstruction<Op>>) -> Vec<ILInstruction<Op>> {
    use il::ILInstruction::*;
    let branch = match &condition[condition.len() - 1] {
        Branch(br) => Branch(branch(invert(&br.type_), br.target)),
        _ => panic!("not supported"),
    };
    let last_idx = condition.len() - 1;
    condition[last_idx] = branch;
    condition
}

fn invert(type_: &BranchType) -> BranchType {
    use il::BranchType::*;
    match type_ {
        Equal => NotEqual,
        NotEqual => Equal,
        Less => GreaterOrEqual,
        LessOrEqual => Greater,
        Greater => LessOrEqual,
        GreaterOrEqual => Less,
        Unconditional => panic!("impossible"),
    }
}
