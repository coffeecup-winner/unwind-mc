module LinqExtensions

open System.Collections.Generic

type IEnumerable<'a> with
    member self.ToSet<'a>(): ISet<'a> =
        new HashSet<'a>(self) :> ISet<'a>
