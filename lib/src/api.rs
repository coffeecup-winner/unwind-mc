use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::fs::File;
use std::io::Read;
use std::os::raw::c_char;
use std::path::PathBuf;

use goblin::Object;
use serde_json as json;
use serde_json::json;

use analyzer::Analyzer;

const VERSION: &[u8] = b"0.1.0\0";

thread_local!(
    static OPEN_HANDLES: RefCell<Option<HashMap<u32, Box<Analyzer>>>> = RefCell::new(None));
thread_local!(
    static NEXT_HANDLE: u32 = 1);
const INVALID_HANDLE: u32 = 0;

#[no_mangle]
pub extern "C" fn version() -> *const c_char {
    unsafe {
        return CStr::from_bytes_with_nul_unchecked(VERSION).as_ptr();
    }
}

#[no_mangle]
pub extern "C" fn init() -> bool {
    OPEN_HANDLES.with(|handles|
        *handles.borrow_mut() = Some(HashMap::new())
    );
    true
}

#[no_mangle]
pub extern "C" fn open_binary_file(path: *const c_char) -> u32 {
    // TODO: Move out parts of this function
    let path = match to_str(path).map(|p| PathBuf::from(p)) {
        Some(p) => p,
        None => return INVALID_HANDLE,
    };
    let mut file = match File::open(&path) {
        Ok(f) => f,
        Err(_) => return INVALID_HANDLE,
    };
    let mut buf = vec![];
    file.read_to_end(&mut buf).expect("Failed to read file");
    let mut analyzer = None;
    match Object::parse(&buf).expect("Failed to parse the file contents") {
        Object::Elf(elf) => {
            for section in elf.section_headers {
                if section.is_executable() {
                    let range = section.file_range();
                    let vm_range = section.vm_range();
                    let text = Vec::from(&buf[range]);
                    analyzer = Some(Analyzer::create(text, vm_range.start as u64)
                        .expect("Failed to create the analyze"));
                }
            }
        }
        Object::PE(pe) => return INVALID_HANDLE,
        _ => return INVALID_HANDLE,
    }
    return match analyzer {
        Some(analyzer) => {
            let handle = NEXT_HANDLE.with(|next_cell| {
                let handle = *next_cell;
                OPEN_HANDLES.with(|handles_cell|
                    handles_cell.borrow_mut().as_mut().unwrap().insert(handle, Box::new(analyzer))
                );
                handle
            });
            handle
        }
        None => INVALID_HANDLE
    }
}

#[no_mangle]
pub fn print_instructions(handle: u32, ptr: *mut c_char, size: usize) {
    OPEN_HANDLES.with(|handles_cell| {
        let handles = handles_cell.borrow();
        let analyzer = &handles.as_ref().unwrap()[&handle];
        let mut asm = vec![];
        for (_, insn) in analyzer.graph().instructions_iter() {
            asm.push(json!({
                "address": insn.address,
                "hex": insn.hex.clone(),
                "assembly": insn.assembly.clone(),
            }));
        }
        let s = json::Value::Array(asm).to_string();
        let count = std::cmp::min(s.len() + 1, size);
        unsafe {
            let data = CString::from_vec_unchecked(s.into_bytes());
            std::ptr::copy_nonoverlapping(data.as_ptr(), ptr, count - 1);
            std::slice::from_raw_parts_mut(ptr, count)[count - 1] = 0;
        }
    });
}

fn to_str<'a>(s: *const c_char) -> Option<&'a str> {
    let cstr = unsafe { CStr::from_ptr(s) };
    match cstr.to_str() {
        Ok(s) => Some(s),
        Err(_) => None,
    }
}