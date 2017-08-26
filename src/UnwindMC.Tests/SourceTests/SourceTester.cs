using System;
using System.IO;
using System.Linq;
using NUnit.Framework;

namespace UnwindMC.Tests.SourceTests
{
    class SourceTester
    {
        public static void TestDecompiler(string code, string expected)
        {
            code = Trim(code);

            Console.WriteLine("===================================== CODE =====================================");
            Console.WriteLine(code);
            Console.WriteLine("");

            var asm = Disassemble(code);

            Console.WriteLine("===================================== ASM ======================================");
            Console.WriteLine(asm);
            Console.WriteLine("");

            var cppCode = Decompiler.decompileFunction(AnalysisHelper.Analyze(asm), 0x0);

            Console.WriteLine("==================================== RESULT ====================================");
            Console.WriteLine(cppCode);
            Console.WriteLine("");

            Assert.That(cppCode, Is.EqualTo(Trim(expected)));
        }

        private static string Disassemble(string test)
        {
            using (var tempDir = VcTools.CreateTempDirectory())
            {
                var cppPath = Path.Combine(tempDir.Path, "test.cpp");
                File.WriteAllText(cppPath, test);
                var objPath = VcTools.Compile(cppPath);
                return VcTools.Disassemble(objPath);
            }
        }

        private static string Trim(string expected)
        {
            var lines = expected.Split(new[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries);
            int indent = lines[0].TakeWhile(c => c == ' ').Count();
            expected = string.Join(Environment.NewLine, lines.Select(l => l.Substring(indent)));
            return expected;
        }
    }
}
