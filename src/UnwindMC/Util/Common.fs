[<AutoOpen>]
module Common

type Either<'a, 'b>
    = Left of 'a
    | Right of 'b

module ROL =
    open System.Collections.Generic

    let map (mapping: 'a -> 'b) (list: IReadOnlyList<'a>): IReadOnlyList<'b> =
        list
        |> Seq.map mapping
        |> Seq.toArray
        :> IReadOnlyList<'b>
