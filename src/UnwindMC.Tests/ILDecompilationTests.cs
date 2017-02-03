using NDis86;
using NUnit.Framework;
using UnwindMC.Analysis.IL;

using static UnwindMC.Tests.Helpers.ILHelper;

namespace UnwindMC.Tests
{
    [TestFixture]
    public class ILDecompilationTests
    {
        [Test]
        public void TestILWithFunctionPointers()
        {
            var analyzer = AnalysisHelper.Analyze(@"
                00400000                   56 push esi
                00400001             8b742408 mov esi, [esp+0x8]
                00400005             3b74240c cmp esi, [esp+0xc]
                00400009                 730d jae 0x400018
                0040000b                 8b06 mov eax, [esi]
                0040000d                 85c0 test eax, eax
                0040000f                 7402 jz 0x400013
                00400011                 ffd0 call eax
                00400013               83c604 add esi, 0x4
                00400016                 ebed jmp 0x400005
                00400018                   5e pop esi
                00400019                   c3 ret");
            var il = ILDecompiler.Decompile(analyzer.Graph, 0x400000);

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

            AssertILEqual(asn0, il);
        }
    }
}
