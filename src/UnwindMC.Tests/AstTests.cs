using NDis86;
using NUnit.Framework;
using System.Collections.Generic;
using UnwindMC.Analysis.Ast;
using UnwindMC.Analysis.Data;
using UnwindMC.Analysis.IL;
using static UnwindMC.Tests.Helpers.AstHelper;
using static UnwindMC.Tests.Helpers.FlowHelper;
using static UnwindMC.Tests.Helpers.ILHelper;

namespace UnwindMC.Tests
{
    [TestFixture]
    public class AstTests
    {
        [Test]
        public void TestAstWithFunctionPointers()
        {
            var asn0 = Assign(Register(OperandType.ESI), Stack(0));
            var cmp0 = Compare(Register(OperandType.ESI), Stack(4));
            var asn1 = Assign(Register(OperandType.EAX), Pointer(OperandType.ESI));
            var cmp1 = Compare(Register(OperandType.EAX), Value(0));
            var call = Call(Register(OperandType.EAX));
            var add = Add(Register(OperandType.ESI), Value(4));
            var ret = Return();

            asn0.AddDefaultChild(cmp0);
            cmp0.AddDefaultChild(ret);
            cmp0.AddConditionalChild(ILBranchType.Less, asn1);
            asn1.AddDefaultChild(cmp1);
            cmp1.AddDefaultChild(add);
            cmp1.AddConditionalChild(ILBranchType.NotEqual, call);
            call.AddDefaultChild(add);
            add.AddDefaultChild(cmp0);

            var blocks = Blocks(
                Sequential(asn0),
                Loop(cmp0,
                    Sequential(asn1),
                    Conditional(cmp1,
                        Blocks(Sequential(call)),
                        Blocks(Sequential())),
                    Sequential(add)),
                Sequential(ret));

            var types = new Dictionary<ILOperand, Type>
            {
                { Stack(0), new Type(true, 1) },
                { Stack(4), new Type(true, 1) },
            };

            var ast = new AstBuilder(blocks, types).BuildAst();
            var expected = Scope(
                Assign(Var("var"), Var("arg")),
                While(Less(Var("var"), Var("arg")),
                    Scope(
                        Assign(Var("var"), Dereference(Var("var"))),
                        IfThenElse(NotEqual(Var("var"), Val(0)),
                            Scope(Call(Var("var"))),
                            Scope()),
                        Assign(Var("var"), Add(Var("var"), Val(4))))),
                Ret());
            AssertAstEqual(expected, ast);
        }
    }
}
