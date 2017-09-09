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
    children: IReadOnlyList<Block>
}

type SequentialBlock = {
    instructions: IReadOnlyList<ILInstruction>
}

type WhileBlock = {
    condition: IReadOnlyList<ILInstruction>
    children: IReadOnlyList<Block>
}

type Block =
    | ConditionalBlock of ConditionalBlock
    | DoWhileBlock of DoWhileBlock
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
    let result = new List<Block>()
    let sequence = new List<ILInstruction>()
    let rec run: (int * ILInstruction * int option) list -> unit =
        function
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
            if (sequence.Count > 0) then
                result.Add(SequentialBlock { instructions = sequence |> Seq.toArray })
                sequence.Clear()
            result.Add(whileLoop (all |> List.take (end_ - start)))
            run (all |> List.skip (end_ - start + 1))
        (** do while loop **
            ...
            START:
            <body>
            <condition>
            br START
            ...
        *)
        | ((start, _, Some end_) :: _) as all when end_ > start ->
            if (sequence.Count > 0) then
                result.Add(SequentialBlock { instructions = sequence |> Seq.toArray })
                sequence.Clear()
            result.Add(doWhileLoop (end_ - start - 1) (all |> List.take (end_ - start + 1)))
            run (all |> List.skip (end_ - start + 1))
            ()
        (** conditional **
          * if-then-else *  |  * if then *   | * if else *
            ...             |    ...         |   ...
            <neg-cond>      |    <neg-cond>  |   <neg-cond>
            br FALSE        |    br END      |   br FALSE
            <true>          |    <true>      |   jmp END
            jmp END         |    END:        |   FALSE:
            FALSE:          |    ...         |   <false>
            <false>                          |   END:
            END:                             |   ...
            ...
        *)
        | ((start, _, _) :: (_, Branch { target = falseBranch }, _) :: _) as all ->
            if (sequence.Count > 0) then
                result.Add(SequentialBlock { instructions = sequence |> Seq.toArray })
                sequence.Clear()
            let end_ =
                match all |> List.skip ((int)falseBranch - start - 1) with
                | (_, Branch { type_ = Unconditional; target = end_ }, _) :: _ -> (int)end_
                | _ -> (int)falseBranch // no else case
            let (ifThenElse, rest) = all |> List.splitAt (end_ - start)
            result.Add(conditional ((int)falseBranch - start) ifThenElse)
            run rest
        (** sequential flow **
            ...
        *)
        | (_, instr, _) :: rest ->
            sequence.Add(instr)
            run rest
        (** end of scope **
        *)
        | [] ->
            if (sequence.Count > 0) then
                result.Add(SequentialBlock { instructions = sequence |> Seq.toArray })
                sequence.Clear()
    run instructions
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

let doWhileLoop (condition: int) (instructions: (int * ILInstruction * int option) list): Block =
    let (instructions, conditionInstructions) = instructions |> List.splitAt condition
    let body =
        match instructions with
        | (start, instr, _) :: rest -> (start, instr, None) :: rest
        | _ -> impossible
    DoWhileBlock {
        condition = conditionInstructions |> Seq.map (fun (_, i, _) -> i) |> Seq.toArray
        children = scope body
    }

let whileLoop (instructions: (int * ILInstruction * int option) list): Block =
    let (condition, instructions) = instructions |> List.splitAt 2
    let body =
        match instructions with
        | (start, instr, _) :: rest -> (start, instr, None) :: rest
        | _ -> impossible
    WhileBlock {
        condition = invertCondition (condition |> Seq.map (fun (_, i, _) -> i) |> Seq.toArray)
        children = scope body
    }

let invertCondition (condition: ILInstruction[]): ILInstruction[] =
    match condition with
    | [| compare; Branch branch |] -> [| compare; Branch { branch with type_ = invert branch.type_ } |]
    | _ -> notSupported

let invert (type_: BranchType): BranchType =
    match type_ with
    | Equal -> NotEqual
    | NotEqual -> Equal
    | Less -> GreaterOrEqual
    | LessOrEqual -> Greater
    | GreaterOrEqual -> Less
    | Greater -> LessOrEqual
    | Unconditional -> impossible
