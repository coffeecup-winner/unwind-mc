using NDis86;
using NUnit.Framework;
using UnwindMC.Analysis.Flow;
using UnwindMC.Analysis.IL;
using static UnwindMC.Tests.Helpers.FlowHelper;
using static UnwindMC.Tests.Helpers.ILHelper;

namespace UnwindMC.Tests
{
    [TestFixture]
    public class FlowTests
    {
        [Test]
        public void TestFlowWithFunctionPointers()
        {
            var asn0 = Assign(Register(OperandType.ESI), Stack(0));
            var cmp0 = Compare(Register(OperandType.ESI), Stack(4));
            var asn1 = Assign(Register(OperandType.EAX), Pointer(OperandType.ESI));
            var cmp1 = Compare(Register(OperandType.EAX), Value(0));
            var call = Call(Register(OperandType.EAX));
            var add = Add(Register(OperandType.ESI), Value(4));
            var ret = Return();

            SetOrder(asn0, cmp0, asn1, cmp1, call, add, ret);

            asn0.AddDefaultChild(cmp0);
            cmp0.AddDefaultChild(ret);
            cmp0.AddConditionalChild(ILBranchType.Less, asn1);
            asn1.AddDefaultChild(cmp1);
            cmp1.AddDefaultChild(add);
            cmp1.AddConditionalChild(ILBranchType.NotEqual, call);
            call.AddDefaultChild(add);
            add.AddDefaultChild(cmp0);

            var blocks = FlowAnalyzer.Analyze(asn0);

            var expected = Blocks(
                Sequential(asn0),
                While(cmp0,
                    Sequential(asn1),
                    Conditional(cmp1,
                        Blocks(Sequential(call)),
                        Blocks(Sequential())),
                    Sequential(add)),
                Sequential(ret));

            AssertFlowEqual(expected, blocks);            
        }

        [Test]
        public void TestFlowFindMax()
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

            SetOrder(asn0, asn1, cmp0, asn2, asn3, asn4, cmp1, asn5, add0, sub0, cmp2, ret);

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

            var blocks = FlowAnalyzer.Analyze(asn0);

            var expected = Blocks(
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

            AssertFlowEqual(expected, blocks);
        }
    }
}
