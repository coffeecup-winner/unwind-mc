[<NUnit.Framework.TestFixture>]
module IntArithmeticTests

open NUnit.Framework

[<Test>]
let add (): unit =
    let code = """
        int add(int a, int b) {
          return a + b;
        }"""
    let expected = """
        int sub_000000(int arg0, int arg1)
        {
          int var0 = arg0;
          var0 = var0 + arg1;
          return var0;
        }
        """
    SourceTester.testDecompiler code expected

[<Test>]
let subtract (): unit =
    let code = """
        int subtract(int a, int b) {
          return a - b;
        }"""
    let expected = """
        int sub_000000(int arg0, int arg1)
        {
          int var0 = arg0;
          var0 = var0 - arg1;
          return var0;
        }
        """
    SourceTester.testDecompiler code expected

[<Test>]
let negate (): unit =
    let code = """
        int negate(int a) {
          return -a;
        }"""
    let expected = """
        int sub_000000(int arg0)
        {
          int var0 = arg0;
          var0 = -var0;
          return var0;
        }
        """
    SourceTester.testDecompiler code expected

[<Test>]
let multiply (): unit =
    let code = """
        int multiply(int a, int b) {
          return a * b;
        }"""
    let expected = """
        int sub_000000(int arg0, int arg1)
        {
          int var0 = arg0;
          var0 = var0 * arg1;
          return var0;
        }
        """
    SourceTester.testDecompiler code expected

[<Test>]
let divide (): unit =
    let code = """
        int divide(int a, int b) {
          return a / b;
        }"""
    let expected = """
        int sub_000000(int arg0, int arg1)
        {
          int var0 = arg0;
          var0 = var0 / arg1;
          return var0;
        }
        """
    SourceTester.testDecompiler code expected

[<Test; Ignore(TODO.SupportMultipleOutInstructions)>]
let modulo (): unit =
    let code = """
        int divide(int a, int b) {
          return a % b;
        }"""
    let expected = """
        int sub_000000(int arg0, int arg1)
        {
          int var0 = arg0;
          var0 = var0 % arg1;
          return var0;
        }
        """
    SourceTester.testDecompiler code expected

[<Test>]
let not (): unit =
    let code = """
        int not(int a) {
          return ~a;
        }"""
    let expected = """
        int sub_000000(int arg0)
        {
          int var0 = arg0;
          var0 = ~var0;
          return var0;
        }
        """
    SourceTester.testDecompiler code expected

[<Test>]
let ``and`` (): unit =
    let code = """
        int and(int a, int b) {
          return a & b;
        }"""
    let expected = """
        int sub_000000(int arg0, int arg1)
        {
          int var0 = arg0;
          var0 = var0 & arg1;
          return var0;
        }
        """
    SourceTester.testDecompiler code expected

[<Test>]
let ``or`` (): unit =
    let code = """
        int or(int a, int b) {
          return a | b;
        }"""
    let expected = """
        int sub_000000(int arg0, int arg1)
        {
          int var0 = arg0;
          var0 = var0 | arg1;
          return var0;
        }
        """
    SourceTester.testDecompiler code expected

[<Test>]
let xor (): unit =
    let code = """
        int xor(int a, int b) {
          return a ^ b;
        }"""
    let expected = """
        int sub_000000(int arg0, int arg1)
        {
          int var0 = arg0;
          var0 = var0 ^ arg1;
          return var0;
        }
        """
    SourceTester.testDecompiler code expected

[<Test; Ignore(TODO.SupportWordRegisters)>]
let leftShift (): unit =
    let code = """
        int xor(int a, int b) {
          return a << b;
        }"""
    let expected = """
        int sub_000000(int arg0, int arg1)
        {
          int var0 = arg0;
          var0 = var0 << arg1;
          return var0;
        }
        """
    SourceTester.testDecompiler code expected

[<Test; Ignore(TODO.SupportWordRegisters)>]
let rightShift (): unit =
    let code = """
        int xor(int a, int b) {
          return a >> b;
        }"""
    let expected = """
        int sub_000000(int arg0, int arg1)
        {
          int var0 = arg0;
          var0 = var0 >> arg1;
          return var0;
        }
        """
    SourceTester.testDecompiler code expected
