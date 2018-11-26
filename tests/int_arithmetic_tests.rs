extern crate libudis86_sys;
extern crate regex;

extern crate unwind_mc;

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
