use std::env::temp_dir;
use std::fs::{create_dir, remove_dir_all, write};
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

use analysis_helper;
use gcc_tools;

pub fn test_decompiler(code: &str, expected: &str) {
    println!("===================================== CODE =====================================");
    println!("{}", code);
    println!("");

    let asm = disassemble(code);

    println!("===================================== ASM ======================================");
    println!("{}", asm);
    println!("");

    let project = analysis_helper::analyze(&asm);
    let cpp_code = project.decompile_function(project.get_function(0).unwrap());

    println!("==================================== RESULT ====================================");
    println!("{}", cpp_code);
    println!("");

    assert_eq!(cpp_code, analysis_helper::strip_indent(expected));
}

fn disassemble(code: &str) -> String {
    let temp_dir = TempDirectory::new();
    let obj_path = compile(temp_dir.path.clone(), code);
    gcc_tools::disassemble(obj_path)
}

fn compile(mut path: PathBuf, code: &str) -> PathBuf {
    path.push("test.cpp");
    write(&path, code).expect("Failed to write file");
    gcc_tools::compile(path)
}

struct TempDirectory {
    pub path: PathBuf,
}

impl Drop for TempDirectory {
    fn drop(&mut self) {
        remove_dir_all(&self.path).expect("Failed to remove temp directory");
    }
}

impl TempDirectory {
    pub fn new() -> TempDirectory {
        let time = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
        let mut temp_dir_name = temp_dir();
        temp_dir_name.push(format!("unwind_mc_tmp_{}", time.subsec_nanos()));
        create_dir(temp_dir_name.clone()).expect("Failed to create temp directory");
        TempDirectory {
            path: temp_dir_name,
        }
    }
}
