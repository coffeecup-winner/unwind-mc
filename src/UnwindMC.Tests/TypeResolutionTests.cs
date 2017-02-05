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
                Loop(cmp0,
                    Sequential(asn1),
                    Conditional(cmp1,
                        Blocks(Sequential(call)),
                        Blocks(Sequential())),
                    Sequential(add)),
                Sequential(ret));

            var types = TypeResolver.ResolveFunctionArguments(blocks);
            Assert.That(types.Count, Is.EqualTo(2));
            Assert.That(types[Stack(0)].IsFunction, Is.True);
            Assert.That(types[Stack(0)].IndirectionLevel, Is.EqualTo(1));
            Assert.That(types[Stack(4)].IsFunction, Is.True);
            Assert.That(types[Stack(4)].IndirectionLevel, Is.EqualTo(1));
        }
    }
}
