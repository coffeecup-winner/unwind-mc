﻿using NDis86;
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
                00400000: 56            push esi
                00400001: 8b 74 24 08   mov esi, [esp+0x8]
                00400005: 3b 74 24 0c   cmp esi, [esp+0xc]
                00400009: 73 0d         jae 0x400018
                0040000b: 8b 06         mov eax, [esi]
                0040000d: 85 c0         test eax, eax
                0040000f: 74 02         jz 0x400013
                00400011: ff d0         call eax
                00400013: 83 c6 04      add esi, 0x4
                00400016: eb ed         jmp 0x400005
                00400018: 5e            pop esi
                00400019: c3            ret");
            var il = ILDecompiler.Decompile(analyzer.Graph, 0x400000);

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

            AssertILEqual(asn0, il);
        }

        [Test]
        public void TestILFindMax()
        {
            var analyzer = AnalysisHelper.Analyze(@"
                08048400: 56                push esi
                08048401: 8b 4c 24 0c       mov ecx, [esp+0xc]
                08048405: b8 00 00 00 80    mov eax, 0x80000000
                0804840a: 85 c9             test ecx, ecx
                0804840c: 74 16             jz 0x8048424
                0804840e: 8b 54 24 08       mov edx, [esp+0x8]
                08048412: b8 00 00 00 80    mov eax, 0x80000000
                08048417: 8b 32             mov esi, [edx]
                08048419: 39 f0             cmp eax, esi
                0804841b: 0f 4c c6          cmovl eax, esi
                0804841e: 83 c2 04          add edx, 0x4
                08048421: 49                dec ecx
                08048422: 75 f3             jnz 0x8048417
                08048424: 5e                pop esi
                08048425: c3                ret");
            var il = ILDecompiler.Decompile(analyzer.Graph, 0x8048400);

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

            AssertILEqual(asn0, il);
        }
    }
}
