module rec FlowAnalyzer

open System.Collections.Generic
open IL

type Instruction = ILInstruction<ILOperand>

type ConditionalBlock<'op> = {
    condition: IReadOnlyList<ILInstruction<'op>>
    trueBranch: IReadOnlyList<Block<'op>>
    falseBranch : IReadOnlyList<Block<'op>>
}

type DoWhileBlock<'op> = {
    condition: IReadOnlyList<ILInstruction<'op>>
    body: IReadOnlyList<Block<'op>>
}

type ForBlock<'op> = {
    condition: IReadOnlyList<ILInstruction<'op>>
    modifier: IReadOnlyList<ILInstruction<'op>>
    body: IReadOnlyList<Block<'op>>
}

type SequentialBlock<'op> = {
    instructions: IReadOnlyList<ILInstruction<'op>>
}

type WhileBlock<'op> = {
    condition: IReadOnlyList<ILInstruction<'op>>
    body: IReadOnlyList<Block<'op>>
}

type Block<'op> =
    | ConditionalBlock of ConditionalBlock<'op>
    | DoWhileBlock of DoWhileBlock<'op>
    | ForBlock of ForBlock<'op>
    | SequentialBlock of SequentialBlock<'op>
    | WhileBlock of WhileBlock<'op>

type private LoopBounds = {
    condition: int
    pastLoop: int
}

(* Active patterns for parsing the input IL *)

let private (|Br|_|) (instr: Instruction) =
    match instr with
    | Branch b when b.type_ <> Unconditional -> Some (int b.target)
    | _ -> None

let private (|Jmp|_|) (instr: Instruction) =
    match instr with
    | Branch b when b.type_ = Unconditional -> Some (int b.target)
    | _ -> None

let buildFlowGraph (il: IReadOnlyList<Instruction>): List<Block<ILOperand>> =
    let branchTargets = Array.create il.Count None
    for instr in il |> Seq.indexed do
        match instr with
        | address, Branch { target = target } ->
            branchTargets.[int target] <- Some address
        | _ -> ()
    Seq.zip il branchTargets
    |> Seq.indexed
    |> Seq.map (fun (a, (b, c)) -> (a, b, c))
    |> Seq.toList
    |> scope { condition = -1; pastLoop = -1 }

let private scope (loop: LoopBounds) (instructions: (int * Instruction * int option) list): List<Block<ILOperand>> =
    let rec run: (int * Instruction * int option) list -> Either<Instruction, Block<ILOperand>> list =
        function
        (** for loop **
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
        *)
        | (start, Jmp condition, _) :: ((_, _, Some end_) :: _ as rest) ->
            let loop = { condition = start + 1; pastLoop = end_ + 1 }
            let block = forLoop loop (condition - start) (rest |> List.take (end_ - start - 1))
            Right block :: run (rest |> List.skip (end_ - start))
        (** while loop **
            ...
            START:
            <neg-cond>
            br END
            <body>
            jmp START
            END:
            ...
        *)
        | ((start, Compare _, Some end_) :: (_, Br pastLoop, _) :: _) as all ->
            assert (end_ + 1 = pastLoop)
            assert (all |> List.skip (end_ - start) |> List.head |> function (_, Jmp _, _) -> true | _ -> false)
            let loop = { condition = start; pastLoop = pastLoop }
            let block = whileLoop loop (all |> List.take (end_ - start))
            Right block :: run (all |> List.skip (pastLoop - start))
        (** do while loop **
            ...
            START:
            <body>
            <condition>
            br START
            ...
        *)
        | ((start, _, Some end_) :: _) as all when end_ > start ->
            let loop = { condition = end_ - 1; pastLoop = end_ + 1 }
            let block = doWhileLoop loop (loop.pastLoop - start - 2) (all |> List.take (loop.pastLoop - start))
            Right block :: run (all |> List.skip (loop.pastLoop - start))
        (** conditional **
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
        *)
        | ((start, Compare _, _) :: (_, Br falseBranch, _) :: _) as all ->
            let end_ =
                match all |> List.skip (falseBranch - start - 1) with
                | (_, Jmp end_, _) :: _ when end_ <> loop.condition && end_ <> loop.pastLoop -> end_
                | _ -> falseBranch // no else case
            let (ifThenElse, rest) = all |> List.splitAt (end_ - start)
            let block = conditional loop (falseBranch - start) ifThenElse
            Right block :: run rest
        (** loop control **
          * continue *     |  * continue *   |  * break *
            ...            |  ...            |  ...
            LOOP_COND:     |  jmp LOOP_COND  |  jmp LOOP_END
            ...            |  ...            |  ...
            jmp LOOP_COND  |  LOOP_COND:     |  LOOP_END:
            ...            |  ...            |  ...
        *)
        | (_, Jmp target, _) :: rest when target = loop.condition ->
            Left Continue :: run rest
        | (_, Jmp target, _) :: rest when target = loop.pastLoop ->
            Left Break :: run rest
        (** sequential flow **
            ...
        *)
        | (_, instr, _) :: rest ->
            Left instr :: run rest
        (** end of scope **
        *)
        | [] -> []
    let mutable sequence = run instructions
    let result = new List<Block<ILOperand>>()
    while not (sequence.IsEmpty) do
        let instructions =
            sequence
            |> List.takeWhile (function Left _ -> true | _ -> false)
            |> List.map (function Left v -> v | _ -> impossible)
            |> List.toArray
        if instructions.Length > 0 then
            result.Add(SequentialBlock { instructions = instructions })
        sequence <- sequence |> List.skip instructions.Length
        let blocks =
            sequence
            |> List.takeWhile (function Right _ -> true | _ -> false)
            |> List.map (function Right v -> v | _ -> impossible)
            |> List.toArray
        result.AddRange(blocks)
        sequence <- sequence |> List.skip blocks.Length
    result

let private conditional (loop: LoopBounds) (falseBranch: int) (instructions: (int * Instruction * int option) list): Block<ILOperand> =
    if falseBranch = (instructions |> List.length) then
        // no else case
        let (condition, trueBranch) = instructions |> List.splitAt 2
        ConditionalBlock {
            condition = invertCondition (getInstructions condition)
            trueBranch = scope loop trueBranch
            falseBranch = [||]
        }
    else
        let (condition, instructions) = instructions |> List.splitAt 2
        let (trueBranch, instructions) = instructions |> List.splitAt (falseBranch - 3)
        let (_branch, falseBranch) = instructions |> List.splitAt 1
        ConditionalBlock {
            condition = invertCondition (getInstructions condition)
            trueBranch = scope loop trueBranch
            falseBranch = scope loop falseBranch
        }

let private forLoop (loop: LoopBounds) (condition: int) (instructions: (int * Instruction * int option) list): Block<ILOperand> =
    let (modifier, instructions) = instructions |> List.splitAt (condition - 1)
    let bodyStart =
        instructions
        |> List.findIndex (function (_, Br target, _) when target = loop.pastLoop -> true | _ -> false)
        |> (+) 1
    let (condition, body) = instructions |> List.splitAt bodyStart
    ForBlock {
        condition = invertCondition (getInstructions condition)
        modifier = getInstructions modifier
        body = scope loop body
    }

let private doWhileLoop (loop: LoopBounds) (condition: int) (instructions: (int * Instruction * int option) list): Block<ILOperand> =
    let (instructions, conditionInstructions) = instructions |> List.splitAt condition
    let body =
        match instructions with
        | (start, instr, _) :: rest -> (start, instr, None) :: rest
        | _ -> impossible
    DoWhileBlock {
        condition = getInstructions conditionInstructions
        body = scope loop body
    }

let private whileLoop (loop: LoopBounds) (instructions: (int * Instruction * int option) list): Block<ILOperand> =
    let (condition, instructions) = instructions |> List.splitAt 2
    let body =
        match instructions with
        | (start, instr, _) :: rest -> (start, instr, None) :: rest
        | _ -> impossible
    WhileBlock {
        condition = invertCondition (getInstructions condition)
        body = scope loop body
    }

let getInstructions: (int * Instruction * int option) list -> Instruction[] =
    Seq.map (fun (_, i, _) -> i)
    >> Seq.toArray

let invertCondition (condition: Instruction[]): Instruction[] =
    let branch =
        match condition.[condition.Length - 1] with
        | Branch branch -> Branch { branch with type_ = invert branch.type_ }
        | _ -> notSupported
    condition.[condition.Length - 1] <- branch
    condition

let private invert (type_: BranchType): BranchType =
    match type_ with
    | Equal -> NotEqual
    | NotEqual -> Equal
    | Less -> GreaterOrEqual
    | LessOrEqual -> Greater
    | GreaterOrEqual -> Less
    | Greater -> LessOrEqual
    | Unconditional -> impossible
