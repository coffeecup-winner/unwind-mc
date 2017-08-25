using System;
using System.Collections.Generic;
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

            var analyzer = AnalysisHelper.Analyze(asm);
            var function = analyzer.Functions[0x0];
            function.ResolveBody(analyzer.Graph);
            //function.ResolveTypes();
            //function.BuildAst();
            //function.EmitSourceCode();
            var result = TypeResolver.resolveTypes(function.Blocks);
            var ast = AstBuilder.buildAst(function.Blocks, result.parameterTypes, result.localTypes, result.variableTypes);
            // TODO: these should be returned from AST step
            var types = new Dictionary<string, Type.DataType>();
            for (int i = 0; i < result.parameterTypes.Count; i++)
            {
                types.Add("arg" + i, result.parameterTypes[i]);
            }
            for (int i = 0; i < result.localTypes.Count; i++)
            {
                types.Add("loc" + i, result.variableTypes[i]);
            }
            for (int i = 0; i < result.variableTypes.Count; i++)
            {
                types.Add("var" + i, result.variableTypes[i]);
            }
            var cppCode = CppEmitter.emit(function.Name, types, result.parameterTypes.Count, ast);

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
