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
                While(cmp0,
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

        [Test]
        public void TestAstFindMax()
        {
            var asn0 = Assign(Register(OperandType.ECX), Stack(4));
            var asn1 = Assign(Register(OperandType.EAX), Value(int.MinValue));
            var cmp0 = Compare(Register(OperandType.ECX), Value(0));
            var asn2 = Assign(Register(OperandType.EDX), Stack(0));
            var asn3 = Assign(Register(OperandType.EAX), Value(int.MinValue));
            var asn4 = Assign(Register(OperandType.ESI), Pointer(OperandType.EDX));
            var cmp1 = Compare(Register(OperandType.EAX), Register(OperandType.ESI));
            var asn5 = Assign(Register(OperandType.EAX), Register(OperandType.ESI));
            var add0 = Add(Register(OperandType.EDX), Value(4));
            var sub0 = Subtract(Register(OperandType.ECX), Value(1));
            var cmp2 = Compare(Register(OperandType.ECX), Value(0));
            var ret = Return();

            asn0.SetVariableIds(1, -1);
            asn1.SetVariableIds(0, -1);
            cmp0.SetVariableIds(1, -1);
            asn2.SetVariableIds(2, -1);
            asn3.SetVariableIds(0, -1);
            asn4.SetVariableIds(3, 2);
            cmp1.SetVariableIds(0, 3);
            asn5.SetVariableIds(0, 3);
            add0.SetVariableIds(2, -1);
            sub0.SetVariableIds(1, -1);
            cmp2.SetVariableIds(1, -1);
            ret.SetVariableIds(-1, 0);

            asn0.AddDefaultChild(asn1);
            asn1.AddDefaultChild(cmp0);
            cmp0.AddDefaultChild(ret);
            cmp0.AddConditionalChild(ILBranchType.NotEqual, asn2);
            asn2.AddDefaultChild(asn3);
            asn3.AddDefaultChild(asn4);
            asn4.AddDefaultChild(cmp1);
            cmp1.AddDefaultChild(add0);
            cmp1.AddConditionalChild(ILBranchType.Less, asn5);
            asn5.AddDefaultChild(add0);
            add0.AddDefaultChild(sub0);
            sub0.AddDefaultChild(cmp2);
            cmp2.AddDefaultChild(ret);
            cmp2.AddConditionalChild(ILBranchType.NotEqual, asn4);

            var blocks = Blocks(
                Sequential(
                    asn0,
                    asn1),
                Conditional(cmp0,
                    Blocks(
                        Sequential(
                            asn2,
                            asn3),
                        DoWhile(cmp2,
                            Sequential(asn4),
                            Conditional(cmp1,
                                Blocks(Sequential(asn5)),
                                Blocks(Sequential())),
                            Sequential(
                                add0,
                                sub0))),
                    Blocks(Sequential())),
                Sequential(ret));

            var parameterTypes = new List<Type>
            {
                new Type(false, 1),
                new Type(false, 0),
            };

            var variableTypes = new List<Type>
            {
                new Type(false, 0),
                new Type(false, 0),
                new Type(false, 1),
                new Type(false, 0),
            };

            var ast = new AstBuilder(blocks, parameterTypes, variableTypes).BuildAst();
            var expected = Scope(
                Assign(Var("var0"), Var("arg1")),
                Assign(Var("var1"), Val(int.MinValue)),
                IfThenElse(NotEqual(Var("var0"), Val(0)),
                    Scope(
                        Assign(Var("var2"), Var("arg0")),
                        Assign(Var("var1"), Val(int.MinValue)),
                        DoWhile(
                            Scope(
                                Assign(Var("var3"), Dereference(Var("var2"))),
                                IfThenElse(Less(Var("var1"), Var("var3")),
                                    Scope(Assign(Var("var1"), Var("var3"))),
                                    Scope()),
                                Assign(Var("var2"), Add(Var("var2"), Val(1))),
                                Assign(Var("var0"), Subtract(Var("var0"), Val(1)))),
                            NotEqual(Var("var0"), Val(0)))),
                    Scope()),
                Ret(Var("var1")));
            AssertAstEqual(expected, ast);
        }
    }
}
