module rec FlowAnalyzer

open System
open System.Collections.Generic
open System.Linq
open IGraph
open IL
open LinqExtensions

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
    build il null doWhileLoops -1

let private build (il: ILInstruction) (subGraph: ISet<ILInstruction>) (doWhileLoops: Queue<Order>) (conditionToIgnore: int): List<Block> =
    let result = new List<Block>()
    let seq = new List<ILInstruction>()
    let graph = (createILGraph).GetSubgraph(subGraph)
    let bfs = graph.BFS(il) |> Seq.toList
    let rec run (X : ILInstruction list): List<Block> =
        match X with
        | instr :: rest ->
            if doWhileLoops.Count > 0 && doWhileLoops.Peek().childOrder = instr.order then
            // the instructions is the beginning of the do-while loop
                let order = doWhileLoops.Dequeue().order
                let body = graph.BFS(instr).Where(fun i -> i.order <= order).ToList()
                let condition = body.Last()
                body.RemoveAt(body.Count - 1)
                result.Add(SequentialBlock { instructions = seq })
                result.Add(DoWhileBlock { condition = condition; children = build instr (body.ToSet()) doWhileLoops order })
                match condition.defaultChild with
                | Some next ->
                    if graph.Contains(next) then
                        result.AddRange(build next (graph.BFS(next).ToSet()) doWhileLoops conditionToIgnore)
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
                let left = graph.BFS(conditionalChild).ToList()
                let right = graph.BFS(defaultChild).ToList()

                let isConditional = left.[left.Count - 1] = right.[right.Count - 1]
                if isConditional then
                    let mutable i0 = left.Count - 2
                    let mutable i1 = right.Count - 2
                    while i0 >= 0 && i1 >= 0 && left.[i0] = right.[i1] do
                        i0 <- i0 - 1
                        i1 <- i1 - 1
                    i0 <- i0 + 1
                    i1 <- i1 + 1
                    result.Add(
                        ConditionalBlock
                            {
                                condition = instr
                                trueBranch = build (left.[0]) (left.Take(i0).ToSet()) doWhileLoops conditionToIgnore
                                falseBranch = build (right.[0]) (right.Take(i1).ToSet()) doWhileLoops conditionToIgnore
                            })
                    result.AddRange(build (left.[i0]) (left.Skip(i0).ToSet()) doWhileLoops conditionToIgnore)
                    result
                else
                    let leftLoop = left.Any(fun i -> (i.defaultChild.IsSome && i.defaultChild.Value = instr) || (i.conditionalChild.IsSome && i.conditionalChild.Value = instr))
                    if leftLoop then
                        result.Add(WhileBlock { condition = instr; children = build (conditionalChild) (left.ToSet()) doWhileLoops conditionToIgnore })
                        result.AddRange(build (right.[0]) (right.ToSet()) doWhileLoops conditionToIgnore)
                        result
                    else
                        let rightLoop = right.Any(fun i -> (i.defaultChild.IsSome && i.defaultChild.Value = instr) || (i.conditionalChild.IsSome && i.conditionalChild.Value = instr))
                        if rightLoop then
                            result.Add(WhileBlock { condition = instr; children = build (defaultChild) (right.ToSet()) doWhileLoops conditionToIgnore })
                            result.AddRange(build (left.[0]) (left.ToSet()) doWhileLoops conditionToIgnore)
                            result
                        else
                            raise (new InvalidOperationException())
        | [] ->
            result.Add(SequentialBlock { instructions = seq })
            result
    run bfs

let private findDoWhileLoops (il: ILInstruction): Order[] =
    let result = new List<Order>()
    for instr in (createILGraph).BFS(il) do
        match instr.conditionalChild with
        | Some child when child.order < instr.order ->
            result.Add({ childOrder = child.order; order = instr.order })
        | _ -> ()
    result
    |> Seq.sortBy (fun c -> c.childOrder, c.order)
    |> Seq.toArray
