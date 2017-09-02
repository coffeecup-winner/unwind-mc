module AstHelper

open System
open System.Collections.Generic
open NUnit.Framework
open Ast

let rec private assertExprEqual (expected: Expression) (actual: Expression): unit =
    match (expected, actual) with
    | Binary (expOp, expLeft, expRight), Binary (actOp, actLeft, actRight) ->
        Assert.That(actOp, Is.EqualTo(expOp))
        assertExprEqual expLeft actLeft
        assertExprEqual expRight actRight
    | Dereference exp, Dereference act ->
        assertExprEqual exp act
    | Unary (expOp, expExpr), Unary (actOp, actExpr) ->
        Assert.That(actOp, Is.EqualTo(expOp))
        assertExprEqual expExpr actExpr
    | Value exp, Value act ->
        Assert.That(act, Is.EqualTo(exp))
    | VarRef (Var exp), VarRef (Var act) ->
        Assert.That(act, Is.EqualTo(exp));
    | _ -> raise (new NotSupportedException())

let private assertCollectionEqual (assertEqual: 'a -> 'a -> unit) (expected: IReadOnlyList<'a>) (actual: IReadOnlyList<'a>): unit =
    Assert.That(actual.Count, Is.EqualTo(expected.Count))
    expected |> Seq.zip actual
    |> Seq.iter (fun (exp, act) -> assertEqual exp act)

let rec private assertStatementEqual (expected: Statement) (actual: Statement): unit =
    match (expected, actual) with
    | Assignment (Var expVar, expExpr), Assignment (Var actVar, actExpr) ->
        Assert.That(actVar, Is.EqualTo(expVar))
        assertExprEqual expExpr actExpr
    | DoWhile (expBody, expCond), DoWhile (actBody, actCond) ->
        assertCollectionEqual assertStatementEqual expBody actBody
        assertExprEqual expCond actCond
    | FunctionCall exp, FunctionCall act ->
        assertExprEqual exp act
    | IfThenElse (expCond, expTrue, expFalse), IfThenElse (actCond, actTrue, actFalse) ->
        assertExprEqual expCond actCond
        assertCollectionEqual assertStatementEqual expTrue actTrue
        assertCollectionEqual assertStatementEqual expFalse actFalse
    | Return exp, Return act ->
        Assert.That(act, Is.EqualTo(exp))
    | While (expCond, expBody), While (actCond, actBody) ->
        assertExprEqual expCond actCond
        assertCollectionEqual assertStatementEqual expBody actBody
    | _ -> raise (new NotSupportedException())

let assertAstEqual (expected: IReadOnlyList<Statement>) (actual: IReadOnlyList<Statement>): unit =
    assertCollectionEqual assertStatementEqual expected actual
