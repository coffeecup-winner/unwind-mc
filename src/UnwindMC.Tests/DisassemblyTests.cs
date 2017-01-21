using NUnit.Framework;

namespace UnwindMC.Tests
{
    [TestFixture]
    public class DisassemblyTests
    {
        [Test]
        public void TestJumpTableResolving()
        {
            // The dd <address> section in the middle is given as raw bytes to the analyzer,
            // so the test is to have correct instructions disassembled from the input.
            // They are automatically checked against it inside AnalysisHelper.Analyze()
            AnalysisHelper.Analyze(@"
                004b154a           bb03000000 mov ebx, 0x3
                004b154f                 3bc3 cmp eax, ebx
                004b1551                 770b ja 0x4b155e
                004b1553       ff24855f154b00 jmp dword [eax*4+0x4b155f]
                004b155a                   5f pop edi
                004b155b                   5e pop esi
                004b155c                   5b pop ebx
                004b155d                   c9 leave
                004b155e                   c3 ret
                004b155f             5a154b00 dd 004b155a
                004b1563             5b154b00 dd 004b155b
                004b1567             5c154b00 dd 004b155c
                004b156b             5d154b00 dd 004b155d
                004b156f                   55 push ebp
                004b1570                 8bec mov ebp, esp
                004b1572               8b4d0c mov ecx, [ebp+0xc]
                004b1575               ff4904 dec dword [ecx+0x4]");
        }
    }
}
