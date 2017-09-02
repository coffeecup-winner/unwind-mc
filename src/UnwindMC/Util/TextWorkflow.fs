module TextWorkflow

open System
open System.Text

type Settings = {
    indentSize: int
}

type State = {
    indentCache: Map<int, string>
    indentLevel: int
    indent: string
    indentPending: bool
}

type T =
    private T of (Settings * StringBuilder -> State -> State)

type TextControl
    = IncreaseIndent
    | DecreaseIndent
    | NewLine

type TextBuilder() =
    member self.Combine(T fa, T fb): T =
        T (fun sb -> fa sb >> fb sb)

    member self.Delay(f) =
        f ()

    member self.For(values: 'a seq, func: 'a -> T): T =
        T (fun sb state ->
            values
            |> Seq.fold (fun st x -> let (T f) = func x in f sb st) state)

    member self.Yield(v: string): T =
        T (fun (_, sb) state ->
            let state =
                if state.indentPending then
                    sb.Append(state.indent) |> ignore
                    { state with indentPending = false }
                else
                    state
            sb.Append(v) |> ignore
            state)

    member self.Yield(c: TextControl): T =
        match c with
        | IncreaseIndent ->
            T (fun (settings, _) state ->
                let newLevel = state.indentLevel + 1
                match state.indentCache.TryFind(newLevel) with
                | Some indent ->
                    { state with
                        indentLevel = newLevel
                        indent = indent
                    }
                | None ->
                    let indent = new System.String(' ', newLevel * settings.indentSize)
                    { state with
                        indentCache = state.indentCache.Add (newLevel, indent)
                        indentLevel = newLevel
                        indent = indent
                    })
        | DecreaseIndent ->
            T (fun _ state ->
                let newLevel = state.indentLevel - 1
                { state with
                    indentLevel = newLevel
                    indent = state.indentCache.[newLevel]
                })
        | NewLine ->
            T (fun (_, sb) state ->
                sb.Append(Environment.NewLine) |> ignore
                { state with indentPending = true })

    member self.YieldFrom(t: T): T =
        t

    member self.Zero(): T =
        T (fun _ state -> state)

let text = new TextBuilder()

let buildText (settings: Settings) (T f): string =
    let sb = new StringBuilder()
    let st = {
        indentCache = Map<int, string> [0, ""]
        indentLevel = 0
        indent = ""
        indentPending = false
    }
    f (settings, sb) st |> ignore
    sb.ToString()
