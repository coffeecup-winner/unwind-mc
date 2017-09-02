module TextWorkflow

open System
open System.Collections.Generic
open System.Text

type Settings = {
    indentSize: int
}

type State = {
    settings: Settings
    indentCache: Map<int, string>
    indentLevel: int
    indent: string
}

type T =
    T of (StringBuilder -> State -> State)

type TextControl
    = IncreaseIndent
    | DecreaseIndent
    | Indent
    | NewLine

type TextBuilder() =
    member self.Combine(T fa, T fb): T =
        T (fun sb state ->
            let state = fa sb state
            fb sb state)

    member self.Delay(f) =
        f ()

    member self.For(values: 'a seq, func: 'a -> T): T =
        T (fun sb state ->
            use enumerator = values.GetEnumerator()
            let mutable st = state
            while enumerator.MoveNext() do
                match func enumerator.Current with
                | T f -> st <- f sb st
            st)

    member self.Yield(v: string): T =
        T (fun sb state ->
            sb.Append(v) |> ignore
            state)

    member self.Yield(c: TextControl): T =
        match c with
        | IncreaseIndent ->
            T (fun _ state ->
                let newLevel = state.indentLevel + 1
                match state.indentCache.TryFind(newLevel) with
                | Some indent ->
                    { state with
                        indentLevel = newLevel
                        indent = indent
                    }
                | None ->
                    let indent = new System.String(' ', newLevel * state.settings.indentSize)
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
        | Indent ->
            T (fun sb state ->
                sb.Append(state.indent) |> ignore
                state)
        | NewLine ->
            T (fun sb state ->
                sb.Append(Environment.NewLine) |> ignore
                state)

    member self.YieldFrom(t: T): T =
        t

    member self.Zero(): T =
        T (fun _ state -> state)

let text = new TextBuilder()

let buildText (settings: Settings) (t: T): string =
    let sb = new StringBuilder()
    let st = {
        settings = settings
        indentCache = Map<int, string> [0, ""]
        indentLevel = 0
        indent = ""
    }
    match t with
    | T func -> func sb st |> ignore
    sb.ToString()
