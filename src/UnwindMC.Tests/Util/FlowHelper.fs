module FlowHelper

open System
open System.Collections.Generic
open NUnit.Framework
open FlowAnalyzer

let rec assertFlowEqual (expected: IReadOnlyList<Block>) (blocks: IReadOnlyList<Block>): unit =
    Assert.That(blocks.Count, Is.EqualTo(expected.Count))
    for i in [0 .. expected.Count - 1] do
        match (expected.[i], blocks.[i]) with
        | SequentialBlock exp, SequentialBlock act ->
            Assert.That(act.instructions.Count, Is.EqualTo(exp.instructions.Count))
            for j in [0 .. act.instructions.Count - 1] do
                ILHelper.assertILEqual (exp.instructions.[j]) (act.instructions.[j]) 
        | WhileBlock exp, WhileBlock act ->
            ILHelper.assertILEqual exp.condition act.condition
            assertFlowEqual (exp.children) (act.children)
        | DoWhileBlock exp, DoWhileBlock act ->
            ILHelper.assertILEqual exp.condition act.condition
            assertFlowEqual exp.children act.children
        | ConditionalBlock exp, ConditionalBlock act ->
            ILHelper.assertILEqual exp.condition act.condition
            assertFlowEqual exp.trueBranch act.trueBranch
            assertFlowEqual exp.falseBranch act.falseBranch
        | _ -> notSupported
