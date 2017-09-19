[<NUnit.Framework.TestFixture>]
module FlowControlTests

open NUnit.Framework

[<Test>]
let ifThen (): unit =
    let code = """
        int ifThen(int a) {
          int x = 0;
          if (a) {
            x = 1;
          }
          return x;
        }"""
    let expected = """
        int sub_000000(int arg0)
        {
          int loc0;
          int var0;
        
          loc0 = 0;
          if (arg0 != 0)
          {
            loc0 = 1;
          }
          var0 = loc0;
          return var0;
        }
        """
    SourceTester.testDecompiler code expected

[<Test>]
let ifThenElse (): unit =
    let code = """
        int ifThenElse(int a) {
          int x = 0;
          if (a) {
            x = 1;
          } else {
            x = 2;
          }
          return x;
        }"""
    let expected = """
        int sub_000000(int arg0)
        {
          int loc0;
          int var0;
        
          loc0 = 0;
          if (arg0 != 0)
          {
            loc0 = 1;
          }
          else
          {
            loc0 = 2;
          }
          var0 = loc0;
          return var0;
        }
        """
    SourceTester.testDecompiler code expected

[<Test>]
let ifElse(): unit =
    let code = """
        int ifElse(int a) {
          int x = 0;
          if (a) {
          } else {
            x = 2;
          }
          return x;
        }"""
    let expected = """
        int sub_000000(int arg0)
        {
          int loc0;
          int var0;
        
          loc0 = 0;
          if (arg0 != 0)
          {
          }
          else
          {
            loc0 = 2;
          }
          var0 = loc0;
          return var0;
        }
        """
    SourceTester.testDecompiler code expected

[<Test>]
let whileDo (): unit =
    let code = """
        int ifElse(int a) {
          int x = 1;
          while (a) {
            x *= 2;
            --a;
          }
          return x;
        }"""
    let expected = """
        int sub_000000(int arg0)
        {
          int loc0;
          int var0;
          int var1;
          int var2;
        
          loc0 = 1;
          while (arg0 != 0)
          {
            var0 = loc0;
            var0 = var0 << 1;
            loc0 = var0;
            var1 = arg0;
            var1 = var1 - 1;
            arg0 = var1;
          }
          var2 = loc0;
          return var2;
        }
        """
    SourceTester.testDecompiler code expected

[<Test>]
let doWhile (): unit =
    let code = """
        int ifElse(int a) {
          int x = 1;
          do {
            x *= 2;
            --a;
          } while (a);
          return x;
        }"""
    let expected = """
        int sub_000000(int arg0)
        {
          int loc0;
          int var0;
          int var1;
          int var2;
        
          loc0 = 1;
          do
          {
            var0 = loc0;
            var0 = var0 << 1;
            loc0 = var0;
            var1 = arg0;
            var1 = var1 - 1;
            arg0 = var1;
          } while (arg0 != 0);
          var2 = loc0;
          return var2;
        }
        """
    SourceTester.testDecompiler code expected

[<Test>]
let forLoop (): unit =
    let code = """
        int forLoop(int a) {
          int x = 0;
          for (int i = 1; i <= a; i++) {
            x += i;
          }
          return x;
        }"""
    let expected = """
        int sub_000000(int arg0)
        {
          int loc0;
          int loc1;
          int var0;
          int var1;
          int var2;
        
          loc1 = 0;
          loc0 = 1;
          for (; loc0 <= arg0; var0 = loc0, var0 = var0 + 1, loc0 = var0)
          {
            var1 = loc1;
            var1 = var1 + loc0;
            loc1 = var1;
          }
          var2 = loc1;
          return var2;
        }
        """
    SourceTester.testDecompiler code expected

[<Test>]
let breakInWhileDo (): unit =
    let code = """
        int ifElse(int a) {
          int x = 1;
          while (a) {
            x *= 2;
            if (x > 10)
              break;
            --a;
          }
          return x;
        }"""
    let expected = """
        int sub_000000(int arg0)
        {
          int loc0;
          int var0;
          int var1;
        
          loc0 = 1;
          while (arg0 != 0)
          {
            var0 = loc0;
            var0 = var0 << 1;
            loc0 = var0;
            if (loc0 > 10)
            {
              break;
            }
            var0 = arg0;
            var0 = var0 - 1;
            arg0 = var0;
          }
          var1 = loc0;
          return var1;
        }
        """
    SourceTester.testDecompiler code expected

[<Test>]
let breakInDoWhile (): unit =
    let code = """
        int ifElse(int a) {
          int x = 1;
          do {
            x *= 2;
            if (x > 10)
              break;
            --a;
          } while (a);
          return x;
        }"""
    let expected = """
        int sub_000000(int arg0)
        {
          int loc0;
          int var0;
          int var1;
        
          loc0 = 1;
          do
          {
            var0 = loc0;
            var0 = var0 << 1;
            loc0 = var0;
            if (loc0 > 10)
            {
              break;
            }
            var0 = arg0;
            var0 = var0 - 1;
            arg0 = var0;
          } while (arg0 != 0);
          var1 = loc0;
          return var1;
        }
        """
    SourceTester.testDecompiler code expected

[<Test>]
let breakInForLoop (): unit =
    let code = """
        int forLoop(int a) {
          int x = 0;
          for (int i = 1; i <= a; i++) {
            x += i;
            if (x > 10)
              break;
          }
          return x;
        }"""
    let expected = """
        int sub_000000(int arg0)
        {
          int loc0;
          int loc1;
          int var0;
          int var1;
        
          loc1 = 0;
          loc0 = 1;
          for (; loc0 <= arg0; var0 = loc0, var0 = var0 + 1, loc0 = var0)
          {
            var0 = loc1;
            var0 = var0 + loc0;
            loc1 = var0;
            if (loc1 > 10)
            {
              break;
            }
          }
          var1 = loc1;
          return var1;
        }
        """
    SourceTester.testDecompiler code expected
