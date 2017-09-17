[<AutoOpen>]
module MiscExtensions

open System
open System.Linq

type System.String with
    member self.StripIndent(): string =
        let lines = self.Replace("\r\n", "\n").Split('\n')
        let indent =
            lines
                .Skip(1)
                .Min(fun l -> l.TakeWhile(fun c -> c = ' ').Count())
        lines.[0] + Environment.NewLine +
            System.String.Join(Environment.NewLine,
                lines
                    .Skip(1)
                    .Select(fun l -> l.Substring(indent)))
