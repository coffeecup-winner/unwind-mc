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

(* Active patterns for parsing the input IL *)

let (|Br|_|) (instr: ILInstruction) =
    match instr with
    | Branch b when b.type_ <> Unconditional -> Some (int b.target)
    | _ -> None

let (|Jmp|_|) (instr: ILInstruction) =
    match instr with
    | Branch b when b.type_ = Unconditional -> Some (int b.target)
    | _ -> None

let buildFlowGraph (il: IReadOnlyList<ILInstruction>): List<Block> =
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
    |> scope 0

let private scope (pastLoop: int) (instructions: (int * ILInstruction * int option) list): List<Block> =
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
        | (start, Jmp condition, _) :: ((_, _, Some end_) :: _ as rest) ->
            let block = forLoop (end_ + 1) (condition - start) (rest |> List.take (end_ - start - 1))
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
            let block = whileLoop pastLoop (all |> List.take (end_ - start))
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
            let pastLoop = end_ + 1
            let block = doWhileLoop pastLoop (pastLoop - start - 2) (all |> List.take (pastLoop - start))
            Right block :: run (all |> List.skip (pastLoop - start))
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
        | ((start, _, _) :: (_, Br falseBranch, _) :: _) as all ->
            let end_ =
                match all |> List.skip (falseBranch - start - 1) with
                | (_, Jmp end_, _) :: _ when end_ <> pastLoop -> end_
                | _ -> falseBranch // no else case
            let (ifThenElse, rest) = all |> List.splitAt (end_ - start)
            let block = conditional pastLoop (falseBranch - start) ifThenElse
            Right block :: run rest
        (** sequential flow **
            ...
        *)
        | (_, Jmp target, _) :: rest when target = pastLoop ->
            Left Break :: run rest
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

let private conditional (pastLoop: int) (falseBranch: int) (instructions: (int * ILInstruction * int option) list): Block =
    if falseBranch = (instructions |> List.length) then
        // no else case
        let (condition, trueBranch) = instructions |> List.splitAt 2
        ConditionalBlock {
            condition = invertCondition (getInstructions condition)
            trueBranch = scope pastLoop trueBranch
            falseBranch = [||]
        }
    else
        let (condition, instructions) = instructions |> List.splitAt 2
        let (trueBranch, instructions) = instructions |> List.splitAt (falseBranch - 3)
        let (_branch, falseBranch) = instructions |> List.splitAt 1
        ConditionalBlock {
            condition = invertCondition (getInstructions condition)
            trueBranch = scope pastLoop trueBranch
            falseBranch = scope pastLoop falseBranch
        }

let private forLoop (pastLoop: int) (condition: int) (instructions: (int * ILInstruction * int option) list): Block =
    let (modifier, instructions) = instructions |> List.splitAt (condition - 1)
    let bodyStart =
        instructions
        |> List.findIndex (function (_, Br target, _) when target = pastLoop -> true | _ -> false) 
        |> (+) 1
    let (condition, body) = instructions |> List.splitAt bodyStart
    ForBlock {
        condition = invertCondition (getInstructions condition)
        modifier = getInstructions modifier
        body = scope pastLoop body
    }

let private doWhileLoop (pastLoop: int) (condition: int) (instructions: (int * ILInstruction * int option) list): Block =
    let (instructions, conditionInstructions) = instructions |> List.splitAt condition
    let body =
        match instructions with
        | (start, instr, _) :: rest -> (start, instr, None) :: rest
        | _ -> impossible
    DoWhileBlock {
        condition = getInstructions conditionInstructions
        body = scope pastLoop body
    }

let private whileLoop (pastLoop: int) (instructions: (int * ILInstruction * int option) list): Block =
    let (condition, instructions) = instructions |> List.splitAt 2
    let body =
        match instructions with
        | (start, instr, _) :: rest -> (start, instr, None) :: rest
        | _ -> impossible
    WhileBlock {
        condition = invertCondition (getInstructions condition)
        body = scope pastLoop body
    }

let getInstructions: (int * ILInstruction * int option) list -> ILInstruction[] =
    Seq.map (fun (_, i, _) -> i)
    >> Seq.toArray

let invertCondition (condition: ILInstruction[]): ILInstruction[] =
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
