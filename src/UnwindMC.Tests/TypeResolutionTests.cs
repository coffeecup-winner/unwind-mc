using NDis86;
using NUnit.Framework;
using UnwindMC.Analysis.Data;
using UnwindMC.Analysis.IL;
using static UnwindMC.Tests.Helpers.FlowHelper;
using static UnwindMC.Tests.Helpers.ILHelper;

namespace UnwindMC.Tests
{
    [TestFixture]
    public class TypeResolutionTests
    {
        [Test]
        public void TestResolutionWithFunctionPointers()
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
                While(cmp0,
                    Sequential(asn1),
                    Conditional(cmp1,
                        Blocks(Sequential(call)),
                        Blocks(Sequential())),
                    Sequential(add)),
                Sequential(ret));

            var types = TypeResolver.ResolveTypes(blocks);
            var parameterTypes = types.ParameterTypes;
            var variableTypes = types.VariableTypes;
            Assert.That(parameterTypes.Count, Is.EqualTo(2));
            Assert.That(parameterTypes[0].IsFunction, Is.True);
            Assert.That(parameterTypes[0].IndirectionLevel, Is.EqualTo(1));
            Assert.That(parameterTypes[1].IsFunction, Is.True);
            Assert.That(parameterTypes[1].IndirectionLevel, Is.EqualTo(1));

            Assert.That(variableTypes.Count, Is.EqualTo(2));
            Assert.That(variableTypes[0].IsFunction, Is.True);
            Assert.That(variableTypes[0].IndirectionLevel, Is.EqualTo(1));
            Assert.That(variableTypes[1].IsFunction, Is.True);
            Assert.That(variableTypes[1].IndirectionLevel, Is.EqualTo(0));

            AssertVarIds(asn0, 0, -1);
            AssertVarIds(cmp0, 0, -1);
            AssertVarIds(asn1, 1, 0);
            AssertVarIds(cmp1, 1, -1);
            AssertVarIds(call, 1, -1);
            AssertVarIds(add, 0, -1);
            AssertVarIds(ret, -1, -1);
        }

        [Test]
        public void TestResolutionFindMax()
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

            // SetOrder(asn0, asn1, cmp0, asn2, asn3, asn4, cmp1, asn5, add0, sub0, cmp2, ret);

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
                                sub0,
                                cmp2))),
                    Blocks(Sequential())),
                Sequential(ret));

            var types = TypeResolver.ResolveTypes(blocks);
            var parameterTypes = types.ParameterTypes;
            var variableTypes = types.VariableTypes;
            Assert.That(parameterTypes.Count, Is.EqualTo(2));
            Assert.That(parameterTypes[0].IsFunction, Is.False);
            Assert.That(parameterTypes[0].IndirectionLevel, Is.EqualTo(1));
            Assert.That(parameterTypes[1].IsFunction, Is.False);
            Assert.That(parameterTypes[1].IndirectionLevel, Is.EqualTo(0));

            Assert.That(variableTypes.Count, Is.EqualTo(6));
            Assert.That(variableTypes[0].IsFunction, Is.False);
            Assert.That(variableTypes[0].IndirectionLevel, Is.EqualTo(0));
            Assert.That(variableTypes[1].IsFunction, Is.False);
            Assert.That(variableTypes[1].IndirectionLevel, Is.EqualTo(1));
            Assert.That(variableTypes[2].IsFunction, Is.False);
            Assert.That(variableTypes[2].IndirectionLevel, Is.EqualTo(0));
            Assert.That(variableTypes[3].IsFunction, Is.False);
            Assert.That(variableTypes[3].IndirectionLevel, Is.EqualTo(0));
            Assert.That(variableTypes[4].IsFunction, Is.False);
            Assert.That(variableTypes[4].IndirectionLevel, Is.EqualTo(0));
            Assert.That(variableTypes[5].IsFunction, Is.False);
            Assert.That(variableTypes[5].IndirectionLevel, Is.EqualTo(0));

            AssertVarIds(asn0, 0, -1);
            AssertVarIds(asn1, 5, -1);
            AssertVarIds(cmp0, 0, -1);
            AssertVarIds(asn2, 1, -1);
            AssertVarIds(asn3, 4, -1);
            AssertVarIds(asn4, 3, 1);
            AssertVarIds(cmp1, 4, 3); // TODO: don't rewrite the var index inside a loop
            AssertVarIds(asn5, 2, 3);
            AssertVarIds(add0, 1, -1);
            AssertVarIds(sub0, 0, -1);
            AssertVarIds(cmp2, 0, -1);
            AssertVarIds(ret, -1, -1);
        }
    }
}
