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

            asn0.SetVariableIds(0, -1);
            cmp0.SetVariableIds(0, -1);
            asn1.SetVariableIds(1, 0);
            cmp1.SetVariableIds(1, -1);
            call.SetVariableIds(1, -1);
            add.SetVariableIds(0, -1);
            ret.SetVariableIds(-1, -1);

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

            var parameterTypes = new List<Type>
            {
                new Type(true, 1),
                new Type(true, 1),
            };

            var variableTypes = new List<Type>
            {
                new Type(true, 1),
                new Type(true, 0),
            };

            var ast = new AstBuilder(blocks, parameterTypes, variableTypes).BuildAst();
            var expected = Scope(
                Assign(Var("var0"), Var("arg0")),
                While(Less(Var("var0"), Var("arg1")),
                    Scope(
                        Assign(Var("var1"), Dereference(Var("var0"))),
                        IfThenElse(NotEqual(Var("var1"), Val(0)),
                            Scope(Call(Var("var1"))),
                            Scope()),
                        Assign(Var("var0"), Add(Var("var0"), Val(1))))),
                Ret());
            AssertAstEqual(expected, ast);
        }
    }
}
