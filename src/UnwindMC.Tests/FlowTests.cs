using NDis86;
using NUnit.Framework;
using UnwindMC.Analysis.IL;
using UnwindMC.Analysis.Flow;

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

            asn0.AddNext(cmp0);
            cmp0.AddLeft(ILBranchType.GreaterOrEqual, ret);
            cmp0.AddRight(ILBranchType.GreaterOrEqual, asn1);
            asn1.AddNext(cmp1);
            cmp1.AddLeft(ILBranchType.Equal, add);
            cmp1.AddRight(ILBranchType.Equal, call);
            call.AddNext(add);
            add.AddNext(cmp0);

            var blocks = FlowAnalyzer.Analyze(asn0);

            var expected = Blocks(
                Sequential(asn0),
                Loop(
                    Sequential(asn1),
                    Conditional(
                        Blocks(Sequential()),
                        Blocks(Sequential(call))),
                    Sequential(add)),
                Sequential(ret));

            AssertFlowEqual(expected, blocks);            
        }
    }
}
