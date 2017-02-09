using NUnit.Framework;
using System.Collections.Generic;
using UnwindMC.Analysis.Data;
using UnwindMC.Analysis.IL;
using UnwindMC.Emit;
using static UnwindMC.Tests.Helpers.AstHelper;
using static UnwindMC.Tests.Helpers.ILHelper;

namespace UnwindMC.Tests
{
    [TestFixture]
    public class CppEmitterTests
    {
        [Test]
        public void TestEmissionWithFunctionPointers()
        {
            var parameters = new Dictionary<ILOperand, Type>
            {
                { Stack(0), new Type(true, 1) },
                { Stack(4), new Type(true, 1) },
            };

            var body = Scope(
                Assign(Var("var0"), Var("arg0")),
                While(Less(Var("var0"), Var("arg1")),
                    Scope(
                        Assign(Var("var1"), Dereference(Var("var0"))),
                        IfThenElse(NotEqual(Var("var1"), Val(0)),
                            Scope(Call(Var("var1"))),
                            Scope()),
                        Assign(Var("var0"), Add(Var("var0"), Val(4))))),
                Ret());

            var source = new CppEmitter("foo", parameters, body).EmitSourceCode();
            var expected =
@"void foo(void (**arg0)(), void (**arg1)())
{
  auto var0 = arg0;
  while (var0 < arg1)
  {
    auto var1 = *(var0);
    if (var1 != 0)
    {
      var1();
    }
    else
    {
    }
    var0 = var0 + 4;
  }
  return;
}
";
            Assert.That(source, Is.EqualTo(expected));
        }
    }
}
