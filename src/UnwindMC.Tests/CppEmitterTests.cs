using System.Collections.Generic;
using NUnit.Framework;
using UnwindMC.Analysis.Data;
using UnwindMC.Tests.Helpers;
using static UnwindMC.Tests.Helpers.AstHelper;

namespace UnwindMC.Tests
{
    [TestFixture, Ignore("Reimplement tests in F#")]
    public class CppEmitterTests
    {
        [Test]
        public void TestEmissionWithFunctionPointers()
        {
            var types = new Dictionary<string, Type>
            {
                { "arg0", new Type(true, 1) },
                { "arg1", new Type(true, 1) },
                { "var0", new Type(true, 1) },
                { "var1", new Type(true, 0) },
            };

            //var body = Scope(
            //    Assign(Var("var0"), Var("arg0")),
            //    While(Less(Var("var0"), Var("arg1")),
            //        Scope(
            //            Assign(Var("var1"), Dereference(Var("var0"))),
            //            IfThenElse(NotEqual(Var("var1"), Val(0)),
            //                Scope(Call(Var("var1"))),
            //                Scope()),
            //            Assign(Var("var0"), Add(Var("var0"), Val(1))))),
            //    Ret());

            //var source = CppEmitter.emit("foo", types, 2, body);
            //var expected =
            //    @"void foo(void (**arg0)(), void (**arg1)())
            //    {
            //      void (**var0)() = arg0;
            //      while (var0 < arg1)
            //      {
            //        void (*var1)() = *(var0);
            //        if (var1 != 0)
            //        {
            //          var1();
            //        }
            //        var0 = var0 + 1;
            //      }
            //      return;
            //    }
            //    ".StripIndent();
            //AssertSourceCodeEquals(expected, source);
        }

        [Test]
        public void TestEmissionFindMax()
        {
            var types = new Dictionary<string, Type>
            {
                { "arg0", new Type(false, 1) },
                { "arg1", new Type(false, 0) },
                { "var0", new Type(false, 0) },
                { "var1", new Type(false, 0) },
                { "var2", new Type(false, 1) },
                { "var3", new Type(false, 0) },
            };

            //var body = Scope(
            //    Assign(Var("var0"), Var("arg1")),
            //    Assign(Var("var1"), Val(int.MinValue)),
            //    IfThenElse(NotEqual(Var("var0"), Val(0)),
            //        Scope(
            //            Assign(Var("var2"), Var("arg0")),
            //            Assign(Var("var1"), Val(int.MinValue)),
            //            DoWhile(
            //                Scope(
            //                    Assign(Var("var3"), Dereference(Var("var2"))),
            //                    IfThenElse(Less(Var("var1"), Var("var3")),
            //                        Scope(Assign(Var("var1"), Var("var3"))),
            //                        Scope()),
            //                    Assign(Var("var2"), Add(Var("var2"), Val(1))),
            //                    Assign(Var("var0"), Subtract(Var("var0"), Val(1)))),
            //                NotEqual(Var("var0"), Val(0)))),
            //        Scope()),
            //    Ret(Var("var1")));

            //var source = CppEmitter.emit("findMax", types, 2, body);
            //// TODO: resolve return type/value
            //var expected =
            //    @"int findMax(int *arg0, int arg1)
            //    {
            //      int var0 = arg1;
            //      int var1 = -2147483648;
            //      if (var0 != 0)
            //      {
            //        int *var2 = arg0;
            //        var1 = -2147483648;
            //        do
            //        {
            //          int var3 = *(var2);
            //          if (var1 < var3)
            //          {
            //            var1 = var3;
            //          }
            //          var2 = var2 + 1;
            //          var0 = var0 - 1;
            //        } while (var0 != 0);
            //      }
            //      return var1;
            //    }
            //    ".StripIndent();
            //AssertSourceCodeEquals(expected, source);
        }

        private static void AssertSourceCodeEquals(string expected, string actual)
        {
            Assert.That(actual.Replace("\r\n", "\n"), Is.EqualTo(expected.Replace("\r\n", "\n")));
        }
    }
}
