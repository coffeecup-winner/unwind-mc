extern crate libudis86_sys;
extern crate regex;

extern crate unwindmc;

mod analysis_helper;
mod gcc_tools;
mod source_tester;

use source_tester::*;

#[test]
fn add() {
    let code = "
        int add(int a, int b) {
          return a + b;
        }";
    let expected = "
        int sub_000000(int arg0, int arg1)
        {
          int var0;
        
          var0 = arg1;
          var0 = var0 + arg0;
          return var0;
        }
        ";
    test_decompiler(code, expected);
}

#[test]
fn subtract() {
    let code = "
        int subtract(int a, int b) {
          return a - b;
        }";
    let expected = "
        int sub_000000(int arg0, int arg1)
        {
          int var0;
        
          var0 = arg0;
          var0 = var0 - arg1;
          return var0;
        }
        ";
    test_decompiler(code, expected);
}

#[test]
fn negate() {
    let code = "
        int negate(int a) {
          return -a;
        }";
    let expected = "
        int sub_000000(int arg0)
        {
          int var0;
        
          var0 = arg0;
          var0 = -var0;
          return var0;
        }
        ";
    test_decompiler(code, expected);
}

#[test]
fn multiply() {
    let code = "
        int multiply(int a, int b) {
          return a * b;
        }";
    let expected = "
        int sub_000000(int arg0, int arg1)
        {
          int var0;
        
          var0 = arg1;
          var0 = var0 * arg0;
          return var0;
        }
        ";
    test_decompiler(code, expected);
}

#[test]
fn divide() {
    let code = "
        int divide(int a, int b) {
          return a / b;
        }";
    let expected = "
        int sub_000000(int arg0, int arg1)
        {
          int var0;
        
          var0 = arg0;
          var0 = var0 / arg1;
          return var0;
        }
        ";
    test_decompiler(code, expected);
}

#[ignore = "TODO: support multiple-output instructions"]
#[test]
fn modulo() {
    let code = "
        int divide(int a, int b) {
          return a % b;
        }";
    let expected = "
        int sub_000000(int arg0, int arg1)
        {
          int var0;
        
          var0 = arg0;
          var0 = var0 % arg1;
          return var0;
        }
        ";
    test_decompiler(code, expected);
}

#[test]
fn not() {
    let code = "
        int _not(int a) {
          return ~a;
        }";
    let expected = "
        int sub_000000(int arg0)
        {
          int var0;
        
          var0 = arg0;
          var0 = ~var0;
          return var0;
        }
        ";
    test_decompiler(code, expected);
}

#[test]
fn and() {
    let code = "
        int _and(int a, int b) {
          return a & b;
        }";
    let expected = "
        int sub_000000(int arg0, int arg1)
        {
          int var0;
        
          var0 = arg1;
          var0 = var0 & arg0;
          return var0;
        }
        ";
    test_decompiler(code, expected);
}

#[test]
fn or() {
    let code = "
        int _or(int a, int b) {
          return a | b;
        }";
    let expected = "
        int sub_000000(int arg0, int arg1)
        {
          int var0;
        
          var0 = arg1;
          var0 = var0 | arg0;
          return var0;
        }
        ";
    test_decompiler(code, expected);
}

#[test]
fn xor() {
    let code = "
        int _xor(int a, int b) {
          return a ^ b;
        }";
    let expected = "
        int sub_000000(int arg0, int arg1)
        {
          int var0;
        
          var0 = arg1;
          var0 = var0 ^ arg0;
          return var0;
        }
        ";
    test_decompiler(code, expected);
}

#[ignore = "TODO: support word registers"]
#[test]
fn left_shift() {
    let code = "
        int xor(int a, int b) {
          return a << b;
        }";
    let expected = "
        int sub_000000(int arg0, int arg1)
        {
          int var0;
        
          var0 = arg0;
          var0 = var0 << arg1;
          return var0;
        }
        ";
    test_decompiler(code, expected);
}

#[ignore = "TODO: support word registers"]
#[test]
fn right_shift() {
    let code = "
        int xor(int a, int b) {
          return a >> b;
        }";
    let expected = "
        int sub_000000(int arg0, int arg1)
        {
          int var0;
        
          var0 = arg0;
          var0 = var0 >> arg1;
          return var0;
        }
        ";
    test_decompiler(code, expected);
}
