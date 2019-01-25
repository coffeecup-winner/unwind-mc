use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::path::PathBuf;

use elf;

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
    let file = match elf::File::open_path(&path) {
        Ok(f) => f,
        Err(_) => return INVALID_HANDLE,
    };
    let text = match file.get_section(".text") {
        Some(s) => s,
        None => return INVALID_HANDLE,
    };
    let analyzer = match Analyzer::create(text.data.clone(), 0) {
        Ok(a) => a,
        Err(_) => return INVALID_HANDLE,
    };
    let handle = NEXT_HANDLE.with(|next_cell| {
        let handle = *next_cell;
        OPEN_HANDLES.with(|handles_cell|
            handles_cell.borrow_mut().as_mut().unwrap().insert(handle, Box::new(analyzer))
        );
        handle
    });
    return handle;
}

#[no_mangle]
pub fn print_instructions(handle: u32, ptr: *mut c_char, size: usize) {
    OPEN_HANDLES.with(|handles_cell| {
        let handles = handles_cell.borrow();
        let analyzer = &handles.as_ref().unwrap()[&handle];
        let mut s = String::from("[");
        for (_, insn) in analyzer.graph().instructions_iter() {
            s += &format!("\"{}\",", insn.assembly);
        }
        s.truncate(s.len() - 1);
        s += "]";
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
