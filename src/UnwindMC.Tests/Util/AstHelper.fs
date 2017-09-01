module AstHelper

open System
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

let rec assertAstEqual (expected: Statement) (actual: Statement): unit =
    match (expected, actual) with
    | Assignment (Var expVar, expExpr), Assignment (Var actVar, actExpr) ->
        Assert.That(actVar, Is.EqualTo(expVar))
        assertExprEqual expExpr actExpr
    | DoWhile (expBody, expCond), DoWhile (actBody, actCond) ->
        assertAstEqual expBody actBody
        assertExprEqual expCond actCond
    | FunctionCall exp, FunctionCall act ->
        assertExprEqual exp act
    | IfThenElse (expCond, expTrue, expFalse), IfThenElse (actCond, actTrue, actFalse) ->
        assertExprEqual expCond actCond
        assertAstEqual expTrue actTrue
        assertAstEqual expFalse actFalse
    | Return exp, Return act ->
        Assert.That(act, Is.EqualTo(exp))
    | Scope exp, Scope act ->
        Assert.That(act.Count, Is.EqualTo(exp.Count))
        exp |> Seq.zip act
        |> Seq.iter (fun (exp, act) -> assertAstEqual exp act)
    | While (expCond, expBody), While (actCond, actBody) ->
        assertExprEqual expCond actCond
        assertAstEqual expBody actBody
    | _ -> raise (new NotSupportedException())
