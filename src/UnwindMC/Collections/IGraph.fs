module IGraph

open System
open System.Collections.Generic
open System.Linq
open NLog

type IGraph<'vid, 'v, 'e> =
    abstract member Contains: 'vid -> bool
    abstract member GetVertex: 'vid -> 'v
    abstract member GetAdjacent: 'vid -> IEnumerable<Either<'vid * 'e, string>>

    abstract member GetSubgraph: ISet<'v> -> IGraph<'vid, 'v, 'e>
    abstract member WithEdgeFilter: Func<'e, bool> -> IGraph<'vid, 'v, 'e>
    abstract member ReverseEdges: unit -> IGraph<'vid, 'v, 'e>

let logger = LogManager.GetCurrentClassLogger()

[<AutoOpen>]
module GraphExtensions =
    type IGraph<'vid, 'v, 'e> with
        member graph.DFS(start: 'vid, consume: 'v -> 'e -> bool): bool =
            let stack = new Stack<'vid * 'e>()
            let visited = new HashSet<'vid>()
            stack.Push((start, Unchecked.defaultof<'e>))
            visited.Add(start) |> ignore
            let mutable visitedAllEdges = true
            while stack.Count > 0 do
                let (vertexId, edge) = stack.Pop()
                if consume (graph.GetVertex(vertexId)) edge then
                    for adj in graph.GetAdjacent(vertexId).Reverse() do
                        match adj with
                        | Right message ->
                            logger.Warn(message)
                            visitedAllEdges <- false
                        | Left (vertex, edge) ->
                            if not (visited.Contains(vertex)) then
                                stack.Push((vertex, edge))
                                visited.Add(vertex) |> ignore
            visitedAllEdges

        member graph.BFS(start: 'vid): IEnumerable<'v> =
            seq {
                if graph.Contains(start) then
                    let queue = new Queue<'vid>()
                    let visited = new HashSet<'vid>()
                    queue.Enqueue(start)
                    visited.Add(start) |> ignore
                    while queue.Count > 0 do
                        let vertexId = queue.Dequeue()
                        yield graph.GetVertex(vertexId)
                        for adj in graph.GetAdjacent(vertexId) do
                            match adj with
                            | Right message ->
                                logger.Warn(message)
                            | Left (vertex, _) ->
                                if visited.Add(vertex) then
                                    queue.Enqueue(vertex)
            }
