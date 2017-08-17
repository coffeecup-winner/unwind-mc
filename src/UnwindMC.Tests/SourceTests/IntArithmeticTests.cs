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
            SourceTester.TestDecompiler(code, expected);
        }

        [Test]
        public void Subtract()
        {
            const string code = @"
                int subtract(int a, int b) {
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
            SourceTester.TestDecompiler(code, expected);
        }

        [Test]
        public void Negate()
        {
            const string code = @"
                int negate(int a) {
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
            SourceTester.TestDecompiler(code, expected);
        }

        [Test]
        public void Multiply()
        {
            const string code = @"
                int multiply(int a, int b) {
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
            SourceTester.TestDecompiler(code, expected);
        }

        [Test]
        public void Divide()
        {
            const string code = @"
                int divide(int a, int b) {
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
            SourceTester.TestDecompiler(code, expected);
        }

        [Test, Ignore(TODO.SupportMultipleOutInstructions)]
        public void Modulo()
        {
            const string code = @"
                int divide(int a, int b) {
                    return a % b;
                }";
            const string expected = @"
                int sub_000000(int arg0, int arg1)
                {
                  int var0 = arg0;
                  var0 = var0 % arg1;
                  return var0;
                }
                ";
            SourceTester.TestDecompiler(code, expected);
        }

        [Test]
        public void Not()
        {
            const string code = @"
                int not(int a) {
                    return ~a;
                }";
            const string expected = @"
                int sub_000000(int arg0)
                {
                  int var0 = arg0;
                  var0 = ~var0;
                  return var0;
                }
                ";
            SourceTester.TestDecompiler(code, expected);
        }

        [Test]
        public void And()
        {
            const string code = @"
                int and(int a, int b) {
                    return a & b;
                }";
            const string expected = @"
                int sub_000000(int arg0, int arg1)
                {
                  int var0 = arg0;
                  var0 = var0 & arg1;
                  return var0;
                }
                ";
            SourceTester.TestDecompiler(code, expected);
        }

        [Test]
        public void Or()
        {
            const string code = @"
                int or(int a, int b) {
                    return a | b;
                }";
            const string expected = @"
                int sub_000000(int arg0, int arg1)
                {
                  int var0 = arg0;
                  var0 = var0 | arg1;
                  return var0;
                }
                ";
            SourceTester.TestDecompiler(code, expected);
        }

        [Test]
        public void Xor()
        {
            const string code = @"
                int xor(int a, int b) {
                    return a ^ b;
                }";
            const string expected = @"
                int sub_000000(int arg0, int arg1)
                {
                  int var0 = arg0;
                  var0 = var0 ^ arg1;
                  return var0;
                }
                ";
            SourceTester.TestDecompiler(code, expected);
        }

        [Test, Ignore(TODO.SupportWordRegisters)]
        public void LeftShift()
        {
            const string code = @"
                int xor(int a, int b) {
                    return a << b;
                }";
            const string expected = @"
                int sub_000000(int arg0, int arg1)
                {
                  int var0 = arg0;
                  var0 = var0 << arg1;
                  return var0;
                }
                ";
            SourceTester.TestDecompiler(code, expected);
        }

        [Test, Ignore(TODO.SupportWordRegisters)]
        public void RightShift()
        {
            const string code = @"
                int xor(int a, int b) {
                    return a >> b;
                }";
            const string expected = @"
                int sub_000000(int arg0, int arg1)
                {
                  int var0 = arg0;
                  var0 = var0 >> arg1;
                  return var0;
                }
                ";
            SourceTester.TestDecompiler(code, expected);
        }
    }
}
