module AnalysisHelper

open System
open System.Collections.Generic
open System.Globalization
open System.Linq
open System.Text
open System.Text.RegularExpressions
open Moq
open NUnit.Framework

let NormalLineRegex =
    new Regex(@"^(?<address>[0-9A-F]{8}): (((?<bytes>[0-9A-F]{2})|  ) ){6} (?<asm>.+)$", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)
let AdditionalBytesLineRegex =
    new Regex(@"^(?<bytes>[0-9A-F]{2})( (?<bytes>[0-9A-F]{2}))*$", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)

let analyze (func: string): InstructionGraph.T =
    let lines =
        func
            .Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
            .SkipWhile(System.String.IsNullOrWhiteSpace)
            .Select(fun l -> l.Trim())
            .ToArray()

    let bytes = new List<byte>()
    let canonLines = new List<string>()
    let rec run (match_: System.Text.RegularExpressions.Match) (lines: string list): unit =
        match lines with
        | line :: rest ->
            let addressText = match_.Groups.["address"].Value
            let hex = new StringBuilder()
            for capture in match_.Groups.["bytes"].Captures do
                let b = Byte.Parse(capture.ToString(), NumberStyles.HexNumber)
                bytes.Add(b) |> ignore
                hex.Append(sprintf "%02x" b) |> ignore
            let mutable nextMatch = match_
            let mutable nextLines = rest
            nextMatch <- NormalLineRegex.Match(line)
            if not nextMatch.Success then
                nextMatch <- AdditionalBytesLineRegex.Match(line)
                if not nextMatch.Success then
                    raise (new InvalidOperationException("Line in incorrect format: " + line))
                for capture in nextMatch.Groups.["bytes"].Captures do
                    let b = Byte.Parse(capture.ToString(), NumberStyles.HexNumber)
                    bytes.Add(b) |> ignore
                    hex.Append(sprintf "%02x" b) |> ignore
                match rest with
                | next :: rest ->
                    nextMatch <- NormalLineRegex.Match(next)
                    nextLines <- rest
                    if not nextMatch.Success then
                        raise (new InvalidOperationException("Line in incorrect format: " + next))
                | [] ->
                    nextLines <- []
            canonLines.Add((sprintf "%s %20s" addressText (hex.ToString())).ToLower());
            run nextMatch nextLines
        | [] ->
            let addressText = match_.Groups.["address"].Value
            let hex = new StringBuilder()
            for capture in match_.Groups.["bytes"].Captures do
                let b = Byte.Parse(capture.ToString(), NumberStyles.HexNumber)
                bytes.Add(b) |> ignore
                hex.Append(sprintf "%02x" b) |> ignore
            canonLines.Add((sprintf "%s %20s" addressText (hex.ToString())).ToLower());
    
    let match_ = NormalLineRegex.Match(lines.[0])
    let address = UInt64.Parse(match_.Groups.["address"].Value, NumberStyles.HexNumber)
    run match_ (lines |> Seq.skip 1 |> Seq.toList)

    let analyzer = Analyzer.create (new ArraySegment<byte>(bytes.ToArray())) address (Mock.Of<IImportResolver.IImportResolver>())
    Analyzer.addFunction analyzer address
    Analyzer.analyze(analyzer)

    let graph = Analyzer.getGraph(analyzer)
    Assert.That(graph.Instructions.Select(fun i -> sprintf "%08x %20s" i.Offset i.Hex), Is.EqualTo(canonLines))

    graph
