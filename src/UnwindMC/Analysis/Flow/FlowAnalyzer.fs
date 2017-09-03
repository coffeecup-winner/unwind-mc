module rec FlowAnalyzer

open System.Collections.Generic
open System.Linq
open Graphs
open IL

type Block =
    | ConditionalBlock of ConditionalBlock
    | DoWhileBlock of DoWhileBlock
    | SequentialBlock of SequentialBlock
    | WhileBlock of WhileBlock

type ConditionalBlock = {
    condition: ILInstruction
    trueBranch: IReadOnlyList<Block>
    falseBranch : IReadOnlyList<Block>
}

type DoWhileBlock = {
    condition: ILInstruction
    children: IReadOnlyList<Block>
}

type SequentialBlock = {
    instructions: IReadOnlyList<ILInstruction>
}

type WhileBlock = {
    condition: ILInstruction
    children: IReadOnlyList<Block>
}

type private Order = {
    childOrder: int
    order: int
}

let buildFlowGraph (il: ILInstruction): List<Block> =
    let doWhileLoops = new Queue<Order>()
    for loop in findDoWhileLoops il do
        doWhileLoops.Enqueue(loop)
    build il None doWhileLoops -1

let private build (il: ILInstruction) (subGraph: ISet<ILInstruction> option) (doWhileLoops: Queue<Order>) (conditionToIgnore: int): List<Block> =
    let result = new List<Block>()
    let seq = new List<ILInstruction>()
    let graph = createILGraph |> Graph.subgraph subGraph
    let rec run (X : ILInstruction list): List<Block> =
        match X with
        | instr :: rest ->
            if doWhileLoops.Count > 0 && doWhileLoops.Peek().childOrder = instr.order then
            // the instructions is the beginning of the do-while loop
                let order = doWhileLoops.Dequeue().order
                let body =
                    graph
                    |> Graph.bfs instr
                    |> Seq.where(fun i -> i.order <= order)
                    |> Seq.toMutableList
                let condition = body.Last()
                body.RemoveAt(body.Count - 1)
                result.Add(SequentialBlock { instructions = seq })
                result.Add(DoWhileBlock { condition = condition; children = build instr (Some (body |> Seq.toSet)) doWhileLoops order })
                match condition.defaultChild with
                | Some next ->
                    if graph.Contains(next) then
                        result.AddRange(build next (Some (graph |> Graph.bfs next |> Seq.toSet)) doWhileLoops conditionToIgnore)
                | _ -> ()
                result
            elif instr.conditionalChild.IsNone || instr.order = conditionToIgnore then
                seq.Add(instr)
                run rest
            else
                result.Add(SequentialBlock { instructions = seq })

                let conditionalChild = instr.conditionalChild.Value
                let defaultChild = instr.defaultChild.Value

                // loop detection
                let left = graph |> Graph.bfs conditionalChild |> Seq.toArray
                let right = graph |> Graph.bfs defaultChild |> Seq.toArray

                let isConditional = left.[left.Length - 1] = right.[right.Length - 1]
                if isConditional then
                    let mutable i0 = left.Length - 2
                    let mutable i1 = right.Length - 2
                    while i0 >= 0 && i1 >= 0 && left.[i0] = right.[i1] do
                        i0 <- i0 - 1
                        i1 <- i1 - 1
                    i0 <- i0 + 1
                    i1 <- i1 + 1
                    result.Add(
                        ConditionalBlock
                            {
                                condition = instr
                                trueBranch = build left.[0] (Some (left |> Seq.take i0 |> Seq.toSet)) doWhileLoops conditionToIgnore
                                falseBranch = build right.[0] (Some (right |> Seq.take i1 |> Seq.toSet)) doWhileLoops conditionToIgnore
                            })
                    result.AddRange(build left.[i0] (Some (left |> Seq.skip i0 |> Seq.toSet)) doWhileLoops conditionToIgnore)
                    result
                else
                    let leftLoop = left.Any(fun i -> (i.defaultChild.IsSome && i.defaultChild.Value = instr) || (i.conditionalChild.IsSome && i.conditionalChild.Value = instr))
                    if leftLoop then
                        result.Add(WhileBlock { condition = instr; children = build conditionalChild (Some (left |> Seq.toSet)) doWhileLoops conditionToIgnore })
                        result.AddRange(build right.[0] (Some (right |> Seq.toSet)) doWhileLoops conditionToIgnore)
                        result
                    else
                        let rightLoop = right.Any(fun i -> (i.defaultChild.IsSome && i.defaultChild.Value = instr) || (i.conditionalChild.IsSome && i.conditionalChild.Value = instr))
                        if rightLoop then
                            result.Add(WhileBlock { condition = instr; children = build defaultChild (Some (right |> Seq.toSet)) doWhileLoops conditionToIgnore })
                            result.AddRange(build left.[0] (Some (left |> Seq.toSet)) doWhileLoops conditionToIgnore)
                            result
                        else
                            failwith "Invalid control flow structure"
        | [] ->
            result.Add(SequentialBlock { instructions = seq })
            result
    run (graph |> Graph.bfs il |> Seq.toList)

let private findDoWhileLoops (il: ILInstruction): Order[] =
    let result = new List<Order>()
    for instr in createILGraph |> Graph.bfs il do
        match instr.conditionalChild with
        | Some child when child.order < instr.order ->
            result.Add({ childOrder = child.order; order = instr.order })
        | _ -> ()
    result
    |> Seq.sortBy (fun c -> c.childOrder, c.order)
    |> Seq.toArray
