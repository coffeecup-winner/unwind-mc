[<NUnit.Framework.TestFixture>]
module rec CppEmitterTests

open System
open System.Collections.Generic
open NUnit.Framework
open Ast
open Type
open MiscExtensions

[<Test>]
let testEmissionWithFunctionPointers (): unit =
    let types = new Dictionary<string, DataType>()
    types.Add("arg0", { isFunction = true; indirectionLevel = 1; size = 4 })
    types.Add("arg1", { isFunction = true; indirectionLevel = 1; size = 4 })
    types.Add("var0", { isFunction = true; indirectionLevel = 1; size = 4 })
    types.Add("var1", { isFunction = true; indirectionLevel = 0; size = 4 })

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

    let source = CppEmitter.emit "foo" types 2 body
    let expected =
        """void foo(void (**arg0)(), void (**arg1)())
        {
          void (**var0)() = arg0;
          while (var0 < arg1)
          {
            void (*var1)() = *(var0);
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
    let types = new Dictionary<string, DataType>()
    types.Add("arg0", { isFunction = false; indirectionLevel = 1; size = 4 })
    types.Add("arg1", { isFunction = false; indirectionLevel = 0; size = 4 })
    types.Add("var0", { isFunction = false; indirectionLevel = 0; size = 4 })
    types.Add("var1", { isFunction = false; indirectionLevel = 0; size = 4 })
    types.Add("var2", { isFunction = false; indirectionLevel = 1; size = 4 })
    types.Add("var3", { isFunction = false; indirectionLevel = 0; size = 4 })

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

    let source = CppEmitter.emit "findMax" types 2 body
    let expected =
        """int findMax(int *arg0, int arg1)
        {
          int var0 = arg1;
          int var1 = -2147483648;
          if (var0 != 0)
          {
            int *var2 = arg0;
            var1 = -2147483648;
            do
            {
              int var3 = *(var2);
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
