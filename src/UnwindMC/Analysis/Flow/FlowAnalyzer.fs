module rec FlowAnalyzer

open System.Collections.Generic
open IL

type ConditionalBlock = {
    condition: IReadOnlyList<ILInstruction>
    trueBranch: IReadOnlyList<Block>
    falseBranch : IReadOnlyList<Block>
}

type DoWhileBlock = {
    condition: IReadOnlyList<ILInstruction>
    body: IReadOnlyList<Block>
}

type ForBlock = {
    condition: IReadOnlyList<ILInstruction>
    modifier: IReadOnlyList<ILInstruction>
    body: IReadOnlyList<Block>
}

type SequentialBlock = {
    instructions: IReadOnlyList<ILInstruction>
}

type WhileBlock = {
    condition: IReadOnlyList<ILInstruction>
    body: IReadOnlyList<Block>
}

type Block =
    | ConditionalBlock of ConditionalBlock
    | DoWhileBlock of DoWhileBlock
    | ForBlock of ForBlock
    | SequentialBlock of SequentialBlock
    | WhileBlock of WhileBlock

let buildFlowGraph (il: IReadOnlyList<ILInstruction>): List<Block> =
    let branchTargets = Array.create il.Count None
    for instr in il |> Seq.indexed do
        match instr with
        | address, Branch { target = target } ->
            branchTargets.[(int)target] <- Some address
        | _ -> ()
    Seq.zip il branchTargets
    |> Seq.indexed
    |> Seq.map (fun (a, (b, c)) -> (a, b, c))
    |> Seq.toList
    |> scope

let scope (instructions: (int * ILInstruction * int option) list): List<Block> =
    let rec run: (int * ILInstruction * int option) list -> Either<ILInstruction, Block> list =
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
        | (start, Branch { type_ = Unconditional; target = condition }, _) :: ((_, _, Some end_) :: _ as rest) ->
            let block = Right <| forLoop ((int)condition - start) (end_ + 1) (rest |> List.take (end_ - start - 1))
            block :: run (rest |> List.skip (end_ - start))
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
        | ((start, Compare _, Some end_) :: (_, Branch { target = afterLoop }, _) :: _) as all ->
            assert (end_ + 1 = (int)afterLoop)
            assert (all |> List.skip (end_ - start) |> List.head |> function (_, Branch { type_ = Unconditional }, _) -> true | _ -> false)
            let block = Right <| whileLoop (all |> List.take (end_ - start))
            block :: run (all |> List.skip (end_ - start + 1))
        (** do while loop **
            ...
            START:
            <body>
            <condition>
            br START
            ...
        *)
        | ((start, _, Some end_) :: _) as all when end_ > start ->
            let block = Right <| doWhileLoop (end_ - start - 1) (all |> List.take (end_ - start + 1))
            block :: run (all |> List.skip (end_ - start + 1))
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
        | ((start, _, _) :: (_, Branch { type_ = type_; target = falseBranch }, _) :: _) as all when type_ <> Unconditional ->
            let end_ =
                match all |> List.skip ((int)falseBranch - start - 1) with
                | (_, Branch { type_ = Unconditional; target = end_ }, _) :: _ -> (int)end_
                | _ -> (int)falseBranch // no else case
            let (ifThenElse, rest) = all |> List.splitAt (end_ - start)
            let block = Right <| conditional ((int)falseBranch - start) ifThenElse
            block :: run rest
        (** sequential flow **
            ...
        *)
        | (_, instr, _) :: rest ->
            Left instr :: run rest
        (** end of scope **
        *)
        | [] -> []
    let mutable sequence = run instructions
    let result = new List<Block>()
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

let conditional (falseBranch: int) (instructions: (int * ILInstruction * int option) list): Block =
    if falseBranch = (instructions |> List.length) then
        // no else case
        let (condition, trueBranch) = instructions |> List.splitAt 2
        ConditionalBlock {
            condition = invertCondition (condition |> Seq.map (fun (_, i, _) -> i) |> Seq.toArray)
            trueBranch = scope trueBranch
            falseBranch = [||]
        }
    else
        let (condition, instructions) = instructions |> List.splitAt 2
        let (trueBranch, instructions) = instructions |> List.splitAt (falseBranch - 3)
        let (_branch, falseBranch) = instructions |> List.splitAt 1
        ConditionalBlock {
            condition = invertCondition (condition |> Seq.map (fun (_, i, _) -> i) |> Seq.toArray)
            trueBranch = scope trueBranch
            falseBranch = scope falseBranch
        }

let forLoop (condition: int) (afterLoop: int) (instructions: (int * ILInstruction * int option) list): Block =
    let (modifier, instructions) = instructions |> List.splitAt (condition - 1)
    let bodyStart =
        instructions
        |> List.findIndex (function (_, Branch { target = t }, _) when (int)t = afterLoop -> true | _ -> false) 
        |> (+) 1
    let (condition, body) = instructions |> List.splitAt bodyStart
    ForBlock {
        condition = invertCondition (condition |> Seq.map (fun (_, i, _) -> i) |> Seq.toArray)
        modifier = modifier |> Seq.map (fun (_, i, _) -> i) |> Seq.toArray
        body = scope body
    }

let doWhileLoop (condition: int) (instructions: (int * ILInstruction * int option) list): Block =
    let (instructions, conditionInstructions) = instructions |> List.splitAt condition
    let body =
        match instructions with
        | (start, instr, _) :: rest -> (start, instr, None) :: rest
        | _ -> impossible
    DoWhileBlock {
        condition = conditionInstructions |> Seq.map (fun (_, i, _) -> i) |> Seq.toArray
        body = scope body
    }

let whileLoop (instructions: (int * ILInstruction * int option) list): Block =
    let (condition, instructions) = instructions |> List.splitAt 2
    let body =
        match instructions with
        | (start, instr, _) :: rest -> (start, instr, None) :: rest
        | _ -> impossible
    WhileBlock {
        condition = invertCondition (condition |> Seq.map (fun (_, i, _) -> i) |> Seq.toArray)
        body = scope body
    }

let invertCondition (condition: ILInstruction[]): ILInstruction[] =
    let branch =
        match condition.[condition.Length - 1] with
        | Branch branch -> Branch { branch with type_ = invert branch.type_ }
        | _ -> notSupported
    condition.[condition.Length - 1] <- branch
    condition

let invert (type_: BranchType): BranchType =
    match type_ with
    | Equal -> NotEqual
    | NotEqual -> Equal
    | Less -> GreaterOrEqual
    | LessOrEqual -> Greater
    | GreaterOrEqual -> Less
    | Greater -> LessOrEqual
    | Unconditional -> impossible
