module Graphs

open System.Collections.Generic
open System.Linq
open NLog

type IGraph<'vid, 'v, 'e> =
    abstract member Contains: 'vid -> bool
    abstract member GetVertex: 'vid -> 'v
    abstract member GetAdjacent: 'vid -> Either<'vid * 'e, string> seq

    abstract member GetSubgraph: ISet<'v> option -> IGraph<'vid, 'v, 'e>
    abstract member WithEdgeFilter: ('e -> bool) -> IGraph<'vid, 'v, 'e>
    abstract member ReverseEdges: unit -> IGraph<'vid, 'v, 'e>

let logger = LogManager.GetCurrentClassLogger()

type Pick<'a> =
    | Return of 'a option
    | Continue

module Graph =
    let subgraph (vertices: ISet<'v> option) (graph: IGraph<'vid, 'v, 'e>): IGraph<'vid, 'v, 'e> =
        graph.GetSubgraph(vertices)

    let withEdgeFilter (predicate: 'e -> bool) (graph: IGraph<'vid, 'v, 'e>): IGraph<'vid, 'v, 'e> =
        graph.WithEdgeFilter(predicate)

    let reverseEdges (graph: IGraph<'vid, 'v, 'e>): IGraph<'vid, 'v, 'e> =
        graph.ReverseEdges()

    let dfsWith (start: 'vid) (consume: 'v -> 'e -> bool) (graph: IGraph<'vid, 'v, 'e>): bool =
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

    let dfsPick (start: 'vid) (pick: 'v -> 'e -> Pick<'a>) (graph: IGraph<'vid, 'v, 'e>): 'a option =
        let mutable result = None
        graph |> dfsWith start (fun v e ->
            match pick v e with
            | Return r ->
                result <- r
                false
            | Continue ->
                true
        ) |> ignore
        result

    let dfs (start: 'vid) (graph: IGraph<'vid, 'v, 'e>): 'v seq =
        // TODO: can be made lazy
        let result = new List<'v>()
        graph |> dfsPick start (fun v e ->
            result.Add(v)
            Continue
        ) |> ignore
        result :> 'v seq

    let bfs (start: 'vid) (graph: IGraph<'vid, 'v, 'e>): 'v seq =
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
