using NUnit.Framework;

namespace UnwindMC.Tests.SourceTests
{
    [TestFixture]
    class FlowControlTests
    {
        [Test]
        public void IfThen()
        {
            const string code = @"
                int ifThen(int a) {
                    int x = 0;
                    if (a) {
                        x = 1;
                    }
                    return x;
                }";
            // TODO: zeroing out local variables should not expose `and`, the code below is incorrect
            const string expected = @"
                int sub_000000(int arg0)
                {
                  int loc0 = loc0 & 0;
                  if (arg0 != 0)
                  {
                    loc0 = 1;
                  }
                  int var0 = loc0;
                  return var0;
                }
                ";
            SourceTester.TestDecompiler(code, expected);
        }

        [Test]
        public void IfThenElse()
        {
            const string code = @"
                int ifThenElse(int a) {
                    int x = 0;
                    if (a) {
                        x = 1;
                    } else {
                        x = 2;
                    }
                    return x;
                }";
            // TODO: zeroing out local variables should not expose `and`, the code below is incorrect
            const string expected = @"
                int sub_000000(int arg0)
                {
                  int loc0 = loc0 & 0;
                  if (arg0 != 0)
                  {
                    loc0 = 1;
                  }
                  else
                  {
                    loc0 = 2;
                  }
                  int var0 = loc0;
                  return var0;
                }
                ";
            SourceTester.TestDecompiler(code, expected);
        }

        [Test]
        public void IfElse()
        {
            const string code = @"
                int ifElse(int a) {
                    int x = 0;
                    if (a) {
                    } else {
                        x = 2;
                    }
                    return x;
                }";
            // TODO: zeroing out local variables should not expose `and`, the code below is incorrect
            const string expected = @"
                int sub_000000(int arg0)
                {
                  int loc0 = loc0 & 0;
                  if (arg0 == 0)
                  {
                    loc0 = 2;
                  }
                  int var0 = loc0;
                  return var0;
                }
                ";
            SourceTester.TestDecompiler(code, expected);
        }
    }
}
