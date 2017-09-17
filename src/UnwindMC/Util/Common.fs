﻿[<AutoOpen>]
module Common

open System
open System.Collections.Generic

type Either<'a, 'b> =
    | Left of 'a
    | Right of 'b

module ROL =
    let map (mapping: 'a -> 'b) (list: IReadOnlyList<'a>): IReadOnlyList<'b> =
        list
        |> Seq.map mapping
        |> Seq.toArray
        :> IReadOnlyList<'b>

module Seq =
    let toSet (collection: 'a seq): ISet<'a> =
        new HashSet<'a>(collection) :> ISet<'a>

    let toMutableList (collection: 'a seq): List<'a> =
        new List<'a>(collection)

let getValue (dict: IReadOnlyDictionary<'a, 'b>) (key: 'a): 'b option =
    match dict.TryGetValue(key) with
    | true, value -> Some value
    | false, _ -> None

let impossible<'a> : 'a =
    failwith "Should not ever happen"

let notSupported<'a> : 'a =
    raise (new NotSupportedException())

let notSupportedWith<'a> (message: string): 'a =
    raise (new NotSupportedException(message))

let FIXME<'a> (message: string): 'a =
    failwithf "FIXME: %s" message
