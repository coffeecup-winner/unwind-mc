﻿[<NUnit.Framework.TestFixture>]
module rec CppEmitterTests

open System
open System.Collections.Generic
open NUnit.Framework
open Ast
open Type
open MiscExtensions

[<Test>]
let testEmissionWithFunctionPointers (): unit =
    let body =
        [|
            Assignment (Var "var0", VarRef (Var "arg0"))
            While
                (
                    Binary (Operator.Less, VarRef (Var "var0"), VarRef (Var "arg1")),
                    [|
                        Assignment (Var "var1", Dereference (VarRef (Var "var0")))
                        IfThenElse
                            (
                                Binary (Operator.NotEqual, VarRef (Var "var1"), Expression.Value 0),
                                [| FunctionCall (VarRef (Var "var1")) |],
                                [||]
                            )
                        Assignment (Var "var0", Binary (Operator.Add, VarRef (Var "var0"), Expression.Value 1))
                    |]
                )
            Statement.Return None
        |]

    let source =
        CppEmitter.emit {
            name = "foo"
            parameters =
                [|
                    { name = "arg0"; type_ = { isFunction = true; indirectionLevel = 1; size = 4 } }
                    { name = "arg1"; type_ = { isFunction = true; indirectionLevel = 1; size = 4 } }
                |]
            locals = [||]
            variables =
                [|
                    { name = "var0"; type_ = { isFunction = true; indirectionLevel = 1; size = 4 } }
                    { name = "var1"; type_ = { isFunction = true; indirectionLevel = 0; size = 4 } }
                |]
            body = body
        }
    let expected =
        """void foo(void (**arg0)(), void (**arg1)())
        {
          void (**var0)();
          void (*var1)();
        
          var0 = arg0;
          while (var0 < arg1)
          {
            var1 = *(var0);
            if (var1 != 0)
            {
              var1();
            }
            var0 = var0 + 1;
          }
          return;
        }
        """.StripIndent()
    assertSourceCodeEquals expected source

[<Test>]
let testEmissionFindMax (): unit =
    let body =
        [|
            Assignment (Var "var0", VarRef (Var "arg1"))
            Assignment (Var "var1", Expression.Value Int32.MinValue)
            IfThenElse
                (
                    Binary (Operator.NotEqual, VarRef (Var "var0"), Expression.Value 0),
                    [|
                        Assignment (Var "var2", VarRef (Var "arg0"))
                        Assignment (Var "var1", Expression.Value Int32.MinValue)
                        DoWhile
                            (
                                [|
                                    Assignment (Var "var3", Dereference (VarRef (Var "var2")))
                                    IfThenElse
                                        (
                                            Binary (Operator.Less, VarRef (Var "var1"), VarRef (Var "var3")),
                                            [| Assignment (Var "var1", VarRef (Var "var3")) |],
                                            [||]
                                        )
                                    Assignment (Var "var2", Binary (Operator.Add, VarRef (Var "var2"), Expression.Value 1))
                                    Assignment (Var "var0", Binary (Operator.Subtract, VarRef (Var "var0"), Expression.Value 1))
                                |],
                                Binary (Operator.NotEqual, VarRef (Var "var0"), Expression.Value 0)
                            )
                    |],
                    [||]
                )
            Statement.Return (Some (Var "var1"))
        |]

    let source =
        CppEmitter.emit {
            name = "findMax"
            parameters =
                [|
                    { name = "arg0"; type_ = { isFunction = false; indirectionLevel = 1; size = 4 } }
                    { name = "arg1"; type_ = { isFunction = false; indirectionLevel = 0; size = 4 } }
                |]
            locals = [||]
            variables =
                [|
                    { name = "var0"; type_ =  { isFunction = false; indirectionLevel = 0; size = 4 } }
                    { name = "var1"; type_ =  { isFunction = false; indirectionLevel = 0; size = 4 } }
                    { name = "var2"; type_ =  { isFunction = false; indirectionLevel = 1; size = 4 } }
                    { name = "var3"; type_ =  { isFunction = false; indirectionLevel = 0; size = 4 } }
                |]
            body = body
        }
    let expected =
        """int findMax(int *arg0, int arg1)
        {
          int var0;
          int var1;
          int *var2;
          int var3;
        
          var0 = arg1;
          var1 = -2147483648;
          if (var0 != 0)
          {
            var2 = arg0;
            var1 = -2147483648;
            do
            {
              var3 = *(var2);
              if (var1 < var3)
              {
                var1 = var3;
              }
              var2 = var2 + 1;
              var0 = var0 - 1;
            } while (var0 != 0);
          }
          return var1;
        }
        """.StripIndent()
    assertSourceCodeEquals expected source

let assertSourceCodeEquals (expected: string) (actual: string): unit =
    Assert.That(actual.Replace("\r\n", "\n"), Is.EqualTo(expected.Replace("\r\n", "\n")))
