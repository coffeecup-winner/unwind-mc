use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;

use log::*;
use serde_json as json;
use serde_json::json;

use analyzer::Analyzer;
use decompiler;
use il_decompiler::decompile;

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
    let mut analyzer = match decompiler::open_binary_file(path) {
        Ok(analyzer) => analyzer,
        Err(s) => {
            error!("{}", s);
            return INVALID_HANDLE;
        }
    };
    analyzer.analyze();
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
    with_analyzer(handle, &mut |analyzer| {
        match decompiler::save_db(analyzer, path) {
            Ok(()) => {},
            Err(s) => error!("{}", s),
        }
    });
    trace!("save_db: done");
}

#[no_mangle]
pub fn get_functions(handle: u32, ptr: *mut c_char, size: usize) {
    trace!("get_functions: {}", handle);
    with_analyzer(handle, &mut |analyzer| {
        let mut functions = vec![];
        for (_, func) in analyzer.functions_iter() {
            functions.push(json!({
                "address": func.address,
                "status": format!("{:?}", func.status),
                "callingConvention": format!("{:?}", func.calling_convention),
                "argumentsSize": func.arguments_size,
            }));
        }
        copy_to_buffer(json::Value::Array(functions).to_string(), ptr, size);
    });
    trace!("get_functions: done");
}

#[no_mangle]
pub fn get_instructions(handle: u32, function: u64, ptr: *mut c_char, size: usize) {
    trace!("get_instructions: {}, 0x{:x}", handle, function);
    with_analyzer(handle, &mut |analyzer| {
        let mut asm = vec![];
        for (_, insn) in analyzer.graph().instructions_iter() {
            if function == 0 {
                asm.push(json!({
                    "address": insn.address,
                    "hex": analyzer.graph().get_bytes_as_hex(insn),
                    "assembly": insn.assembly(),
                }));
            } else {
                match analyzer.graph().get_extra_data(insn.address) {
                    Some(data) if data.function_address == function => {
                        asm.push(json!({
                            "address": insn.address,
                            "hex": analyzer.graph().get_bytes_as_hex(insn),
                            "assembly": insn.assembly(),
                        }));
                    },
                    _ => {},
                }
            }
        }
        copy_to_buffer(json::Value::Array(asm).to_string(), ptr, size);
    });
    trace!("get_instructions: done");
}

#[no_mangle]
pub fn decompile_il(handle: u32, function: u64, ptr: *mut c_char, size: usize) -> bool {
    trace!("get_instructions: {}, 0x{:x}", handle, function);
    with_analyzer(handle, &mut |analyzer| {
        match decompile(analyzer.graph(), function) {
            Ok(il) => {
                let mut res = vec![];
                for (i, insn) in il.iter().enumerate() {
                    res.push(json!({
                        "address": i,
                        "text": insn.print_syntax(),
                    }));
                }
                copy_to_buffer(json::Value::Array(res).to_string(), ptr, size);
                trace!("get_instructions: done");
                true
            }
            Err(s) => {
                error!("get_instructions: ERROR: {}", s);
                false
            }
        }
    })
}

fn with_analyzer<T>(handle: u32, func: &mut FnMut(&Analyzer) -> T) -> T {
    OPEN_HANDLES.with(|handles_cell| {
        let handles = handles_cell.borrow();
        let analyzer = &handles.as_ref().unwrap()[&handle];
        func(analyzer)
    })
}

fn to_str<'a>(s: *const c_char) -> Option<&'a str> {
    let cstr = unsafe { CStr::from_ptr(s) };
    match cstr.to_str() {
        Ok(s) => Some(s),
        Err(_) => None,
    }
}

fn copy_to_buffer(s: String, buf: *mut c_char, size: usize) {
    let count = std::cmp::min(s.len() + 1, size);
    unsafe {
        let data = CString::from_vec_unchecked(s.into_bytes());
        std::ptr::copy_nonoverlapping(data.as_ptr(), buf, count - 1);
        std::slice::from_raw_parts_mut(buf, count)[count - 1] = 0;
    }
}
