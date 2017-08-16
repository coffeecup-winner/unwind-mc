using System;
using System.IO;
using System.Linq;
using NUnit.Framework;

namespace UnwindMC.Tests.SourceTests
{
    [TestFixture]
    public class IntArithmeticTests
    {
        [Test]
        public void Add()
        {
            const string code = @"
                int add(int a, int b) {
                    return a + b;
                }";
            const string expected = @"
                int sub_000000(int arg0, int arg1)
                {
                  int var0 = arg0;
                  var0 = var0 + arg1;
                  return var0;
                }
                ";
            TestDecompiler(code, expected);
        }

        [Test]
        public void Subtract()
        {
            const string code = @"
                int add(int a, int b) {
                    return a - b;
                }";
            const string expected = @"
                int sub_000000(int arg0, int arg1)
                {
                  int var0 = arg0;
                  var0 = var0 - arg1;
                  return var0;
                }
                ";
            TestDecompiler(code, expected);
        }

        [Test]
        public void Negate()
        {
            const string code = @"
                int add(int a) {
                    return -a;
                }";
            const string expected = @"
                int sub_000000(int arg0)
                {
                  int var0 = arg0;
                  var0 = -var0;
                  return var0;
                }
                ";
            TestDecompiler(code, expected);
        }

        [Test]
        public void Multiply()
        {
            const string code = @"
                int add(int a, int b) {
                    return a * b;
                }";
            const string expected = @"
                int sub_000000(int arg0, int arg1)
                {
                  int var0 = arg0;
                  var0 = var0 * arg1;
                  return var0;
                }
                ";
            TestDecompiler(code, expected);
        }

        [Test]
        public void Divide()
        {
            const string code = @"
                int add(int a, int b) {
                    return a / b;
                }";
            const string expected = @"
                int sub_000000(int arg0, int arg1)
                {
                  int var0 = arg0;
                  var0 = var0 / arg1;
                  return var0;
                }
                ";
            TestDecompiler(code, expected);
        }

        private static void TestDecompiler(string code, string expected)
        {
            code = Trim(code);

            Console.WriteLine("===================================== CODE =====================================");
            Console.WriteLine(code);
            Console.WriteLine("");

            var asm = Disassemble(code);

            Console.WriteLine("===================================== ASM ======================================");
            Console.WriteLine(asm);
            Console.WriteLine("");

            var analyzer = AnalysisHelper.Analyze(asm);
            var function = analyzer.Functions[0x0];
            function.ResolveBody(analyzer.Graph);
            function.ResolveTypes();
            function.BuildAst();
            function.EmitSourceCode();

            Console.WriteLine("==================================== RESULT ====================================");
            Console.WriteLine(function.Code);
            Console.WriteLine("");

            Assert.That(function.Code, Is.EqualTo(Trim(expected)));
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
