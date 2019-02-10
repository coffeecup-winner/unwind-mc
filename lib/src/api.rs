use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;

use log::*;
use serde_json as json;
use serde_json::json;

use analyzer::Analyzer;
use decompiler;

const VERSION: &[u8] = b"0.1.0\0";

thread_local!(
    static OPEN_HANDLES: RefCell<Option<HashMap<u32, Box<Analyzer>>>> = RefCell::new(None));
thread_local!(
    static NEXT_HANDLE: u32 = 1);
const INVALID_HANDLE: u32 = 0;

struct CallbackLogger {
    callback: extern "C" fn(line: *const c_char) -> (),
}

impl Log for CallbackLogger {
    fn enabled(&self, _metadata: &Metadata) -> bool {
        true
    }

    fn log(&self, record: &Record) {
        let line = format!("[{}] {}: {}",
            time::strftime("%H:%M:%S.%f", &time::now()).expect("Failed to format time"),
            record.level(), record.args());
        unsafe {
            (self.callback)(CString::from_vec_unchecked(line.into_bytes()).as_ptr());
        }
    }

    fn flush(&self) {}
}

#[no_mangle]
pub extern "C" fn version() -> *const c_char {
    unsafe {
        return CStr::from_bytes_with_nul_unchecked(VERSION).as_ptr();
    }
}

#[no_mangle]
pub extern "C" fn init(log_callback: extern "C" fn(line: *const c_char) -> ()) -> bool {
    log::set_boxed_logger(Box::new(CallbackLogger { callback: log_callback }))
        .expect("Failed to set the logger");
    log::set_max_level(LevelFilter::Trace);
    trace!("Initialized logging");
    OPEN_HANDLES.with(|handles|
        *handles.borrow_mut() = Some(HashMap::new())
    );
    true
}

#[no_mangle]
pub extern "C" fn open_binary_file(path: *const c_char) -> u32 {
    let path = match to_str(path) {
        Some(p) => p,
        None => return INVALID_HANDLE,
    };
    trace!("open_binary_file: {}", path);
    let analyzer = match decompiler::open_binary_file(path) {
        Ok(analyzer) => analyzer,
        Err(s) => {
            error!("{}", s);
            return INVALID_HANDLE;
        }
    };
    let handle = NEXT_HANDLE.with(|next_cell| {
        let handle = *next_cell;
        OPEN_HANDLES.with(|handles_cell|
            handles_cell.borrow_mut().as_mut().unwrap().insert(handle, Box::new(analyzer))
        );
        handle
    });
    trace!("open_binary_file: done");
    handle
}

#[no_mangle]
pub extern "C" fn open_db(path: *const c_char) -> u32 {
    let path = match to_str(path) {
        Some(p) => p,
        None => return INVALID_HANDLE,
    };
    trace!("open_db: {}", path);
    let analyzer = match decompiler::open_db(path) {
        Ok(analyzer) => analyzer,
        Err(s) => {
            error!("{}", s);
            return INVALID_HANDLE;
        }
    };
    let handle = NEXT_HANDLE.with(|next_cell| {
        let handle = *next_cell;
        OPEN_HANDLES.with(|handles_cell|
            handles_cell.borrow_mut().as_mut().unwrap().insert(handle, Box::new(analyzer))
        );
        handle
    });
    trace!("open_db: done");
    handle
}

#[no_mangle]
pub extern "C" fn save_db(handle: u32, path: *const c_char) {
    let path = match to_str(path) {
        Some(p) => p,
        None => return (),
    };
    trace!("save_db: {}", path);
    OPEN_HANDLES.with(|handles_cell| {
        let handles = handles_cell.borrow();
        let analyzer = &handles.as_ref().unwrap()[&handle];

        match decompiler::save_db(analyzer, path) {
            Ok(()) => {},
            Err(s) => error!("{}", s),
        }
    });
    trace!("save_db: done");
}

#[no_mangle]
pub fn print_instructions(handle: u32, ptr: *mut c_char, size: usize) {
    trace!("print_instructions: {}", handle);
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
    trace!("print_instructions: done");
}

fn to_str<'a>(s: *const c_char) -> Option<&'a str> {
    let cstr = unsafe { CStr::from_ptr(s) };
    match cstr.to_str() {
        Ok(s) => Some(s),
        Err(_) => None,
    }
}
