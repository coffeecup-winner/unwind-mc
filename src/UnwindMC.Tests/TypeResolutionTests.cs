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

            Assert.That(asn0.TargetId, Is.EqualTo(0));
            Assert.That(asn0.SourceId, Is.EqualTo(-1));
            Assert.That(cmp0.TargetId, Is.EqualTo(0));
            Assert.That(cmp0.SourceId, Is.EqualTo(-1));
            Assert.That(asn1.TargetId, Is.EqualTo(1));
            Assert.That(asn1.SourceId, Is.EqualTo(0));
            Assert.That(cmp1.TargetId, Is.EqualTo(1));
            Assert.That(cmp1.SourceId, Is.EqualTo(-1));
            Assert.That(call.TargetId, Is.EqualTo(1));
            Assert.That(call.SourceId, Is.EqualTo(-1));
            Assert.That(add.TargetId, Is.EqualTo(0));
            Assert.That(add.SourceId, Is.EqualTo(-1));
            Assert.That(ret.TargetId, Is.EqualTo(-1));
            Assert.That(ret.SourceId, Is.EqualTo(-1));
        }
    }
}
