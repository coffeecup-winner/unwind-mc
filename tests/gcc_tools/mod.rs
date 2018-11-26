use std::path::PathBuf;
use std::process::Command;

pub fn compile(mut path: PathBuf) -> PathBuf {
    let filename = path.file_name().unwrap();
    let mut working_dir = path.clone();
    working_dir.pop();
    Command::new("gcc")
        .current_dir(&working_dir)
        .arg("-c")
        .arg("-m32")
        .arg("-Os")
        .arg(&filename)
        .output()
        .expect(&format!("Failed to compile {:?}", path));
    let mut obj_path = path.clone();
    obj_path.set_extension("o");
    assert!(obj_path.exists());
    return obj_path;
}

pub fn disassemble(path: PathBuf) -> String {
    let output = Command::new("objdump")
        .arg("-d")
        .arg("-M")
        .arg("intel-mnemonic")
        .arg(&path)
        .output()
        .expect(&format!("Failed to disassemble {:?}", path));
    String::from_utf8(output.stdout)
        .expect("Failed to parse UTF8 output")
        .lines()
        .skip_while(|s| !s.starts_with("00000000"))
        .skip(1)
        .collect::<Vec<&str>>()
        .join("\n")
}
