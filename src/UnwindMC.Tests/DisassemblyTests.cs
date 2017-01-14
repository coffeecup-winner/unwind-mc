using NUnit.Framework;
using System;
using System.Linq;
using UnwindMC.Analysis;

namespace UnwindMC.Tests
{
    [TestFixture]
    public class DisassemblyTests
    {
        [Test]
        public void TestJumpTableResolving()
        {
            var code = new byte[]
            {
                0xe8, 0x00, 0x00, 0x00, 0x00, // call 0x4b1553
                0xff, 0x24, 0x85, 0x5f, 0x15, 0x4b, 0x00, // jmp dword [eax*4+0x4b155f]
                0x5f, // pop edi
                0x5e, // pop esi
                0x5b, // pop ebx
                0xc9, // leave
                0xc3, // ret
                0x5a, 0x15, 0x4b, 0x00, // address of pop edi
                0x5b, 0x15, 0x4b, 0x00, // address of pop esi
                0x5c, 0x15, 0x4b, 0x00, // address of pop ebx
                0x5d, 0x15, 0x4b, 0x00, // address of leave
                0x55, // push ebp
                0x8b, 0xec, // mov ebp, esp
                0x8b, 0x4d, 0x0c, // mov ecx, [ebp+0xc]
                0xff, 0x49, 0x04, // dec dword [ecx+0x4]
            };
            var analyzer = new Analyzer(new ArraySegment<byte>(code), 0x004b154e, null);
            analyzer.Analyze();
            var graph = analyzer.Graph;
            Assert.That(graph.Instructions.Select(i => i.Assembly), Is.EqualTo(new[] {
                "call 0x4b1553",
                "jmp dword [eax*4+0x4b155f]",
                "pop edi",
                "pop esi",
                "pop ebx",
                "leave",
                "ret",
                "dd 004b155a",
                "dd 004b155b",
                "dd 004b155c",
                "dd 004b155d",
                "push ebp",
                "mov ebp, esp",
                "mov ecx, [ebp+0xc]",
                "dec dword [ecx+0x4]",
            }));
        }
    }
}
