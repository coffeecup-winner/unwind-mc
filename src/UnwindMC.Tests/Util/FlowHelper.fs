module FlowHelper

open System.Collections.Generic
open NUnit.Framework
open FlowAnalyzer

let rec assertFlowEqual (expected: IReadOnlyList<Block>) (blocks: IReadOnlyList<Block>): unit =
    Assert.That(blocks.Count, Is.EqualTo(expected.Count))
    for i in [0 .. expected.Count - 1] do
        match (expected.[i], blocks.[i]) with
        | SequentialBlock exp, SequentialBlock act ->
            Assert.That(act.instructions, Is.EqualTo(exp.instructions))
        | WhileBlock exp, WhileBlock act ->
            Assert.That(act.condition, Is.EqualTo(exp.condition))
            assertFlowEqual (exp.children) (act.children)
        | DoWhileBlock exp, DoWhileBlock act ->
            Assert.That(act.condition, Is.EqualTo(exp.condition))
            assertFlowEqual exp.children act.children
        | ConditionalBlock exp, ConditionalBlock act ->
            Assert.That(act.condition, Is.EqualTo(exp.condition))
            assertFlowEqual exp.trueBranch act.trueBranch
            assertFlowEqual exp.falseBranch act.falseBranch
        | _ -> notSupported
