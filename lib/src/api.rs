use std::cell::RefCell;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;

use log::*;
use serde_json as json;
use serde_json::json;

use flow_analyzer::Block;
use il::*;
use project::*;

const VERSION: &[u8] = b"0.1.0\0";

thread_local!(
    static PROJECT: RefCell<Option<Project>> = RefCell::new(None));

fn with_project<T>(func: &mut FnMut(&Project) -> T) -> T {
    PROJECT.with(|p| func(p.borrow().as_ref().unwrap()))
}

fn with_project_mut<T>(func: &mut FnMut(&mut Project) -> T) -> T {
    PROJECT.with(|p| func(p.borrow_mut().as_mut().unwrap()))
}

struct CallbackLogger {
    callback: extern "C" fn(line: *const c_char) -> (),
}

impl Log for CallbackLogger {
    fn enabled(&self, _metadata: &Metadata) -> bool {
        true
    }

    fn log(&self, record: &Record) {
        let line = format!(
            "[{}] {}: {}",
            time::strftime("%H:%M:%S.%f", &time::now()).expect("Failed to format time"),
            record.level(),
            record.args()
        );
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
    log::set_boxed_logger(Box::new(CallbackLogger {
        callback: log_callback,
    }))
    .expect("Failed to set the logger");
    log::set_max_level(LevelFilter::Trace);
    trace!("Initialized logging");
    PROJECT.with(|p| *p.borrow_mut() = None);
    true
}

#[no_mangle]
pub extern "C" fn open_binary_file(path: *const c_char) -> bool {
    let path = match to_str(path) {
        Some(p) => p,
        None => return false,
    };
    trace!("open_binary_file: {}", path);
    let mut project = match Project::from_binary_file(path) {
        Ok(project) => project,
        Err(s) => {
            error!("{}", s);
            return false;
        }
    };
    project.analyze_asm();
    PROJECT.with(|p| *p.borrow_mut() = Some(project));
    trace!("open_binary_file: done");
    true
}

#[no_mangle]
pub extern "C" fn open_db(path: *const c_char) -> bool {
    let path = match to_str(path) {
        Some(p) => p,
        None => return false,
    };
    trace!("open_db: {}", path);
    let project = match Project::from_serialized_file(path) {
        Ok(project) => project,
        Err(s) => {
            error!("{}", s);
            return false;
        }
    };
    PROJECT.with(|p| *p.borrow_mut() = Some(project));
    trace!("open_db: done");
    true
}

#[no_mangle]
pub extern "C" fn save_db(path: *const c_char) {
    let path = match to_str(path) {
        Some(p) => p,
        None => return (),
    };
    trace!("save_db: {}", path);
    with_project(
        &mut |project| match Project::serialize_to_file(project, path) {
            Ok(()) => {}
            Err(s) => error!("{}", s),
        },
    );
    trace!("save_db: done");
}

#[no_mangle]
pub fn get_functions(ptr: *mut c_char, size: usize) {
    trace!("get_functions");
    with_project(&mut |project| {
        let mut functions = vec![];
        for (_, func) in project.functions_iter() {
            let mut callees = vec![];
            for callee in func.callees.iter() {
                callees.push(callee);
            }
            let mut callers = vec![];
            for caller in func.callers.iter() {
                callers.push(caller);
            }
            functions.push(json!({
                "address": func.address,
                "status": format!("{:?}", func.status),
                "callingConvention": format!("{:?}", func.calling_convention),
                "argumentsSize": func.arguments_size,
                "name": func.name,
                "callees": callees,
                "callers": callers,
            }));
        }
        copy_to_buffer(json::Value::Array(functions).to_string(), ptr, size);
    });
    trace!("get_functions: done");
}

#[no_mangle]
pub fn get_instructions(function: u64, ptr: *mut c_char, size: usize) {
    trace!("get_instructions: 0x{:x}", function);
    with_project(&mut |project| {
        let mut asm = vec![];
        for (_, insn) in project.graph().instructions_iter() {
            if function == 0 {
                asm.push(json!({
                    "address": insn.address,
                    "hex": project.graph().get_bytes_as_hex(insn),
                    "assembly": insn.assembly(),
                }));
            } else {
                match project.graph().get_extra_data(insn.address) {
                    Some(data) if data.function_address == function => {
                        asm.push(json!({
                            "address": insn.address,
                            "hex": project.graph().get_bytes_as_hex(insn),
                            "assembly": insn.assembly(),
                        }));
                    }
                    _ => {}
                }
            }
        }
        copy_to_buffer(json::Value::Array(asm).to_string(), ptr, size);
    });
    trace!("get_instructions: done");
}

#[no_mangle]
pub fn get_il(address: u64, ptr: *mut c_char, size: usize) -> bool {
    trace!("get_il: 0x{:x}", address);
    with_project(&mut |project| {
        let function = match project.get_function(address) {
            Some(f) => f,
            None => {
                error!("Failed to find a function with the given address");
                return false;
            }
        };
        match &function.il {
            Ok(il) => {
                let mut res = vec![];
                for (i, insn) in il.iter().enumerate() {
                    res.push(json!({
                        "address": i,
                        "text": insn.print_syntax(),
                    }));
                }
                copy_to_buffer(json::Value::Array(res).to_string(), ptr, size);
                trace!("get_il: done");
                true
            }
            Err(s) => {
                error!("get_il: ERROR: {}", s);
                false
            }
        }
    })
}

#[no_mangle]
pub fn get_flow_blocks(address: u64, ptr: *mut c_char, size: usize) -> bool {
    trace!("get_flow_blocks: 0x{:x}", address);
    with_project(&mut |project| {
        let function = match project.get_function(address) {
            Some(f) => f,
            None => {
                error!("Failed to find a function with the given address");
                return false;
            }
        };
        match &function.flow {
            Ok(flow) => {
                let res: serde_json::Value = flow.iter().map(|b| serialize_flow_block(b)).collect();
                copy_to_buffer(res.to_string(), ptr, size);
                trace!("get_il: done");
                true
            }
            Err(s) => {
                error!("get_il: ERROR: {}", s);
                false
            }
        }
    })
}

fn serialize_flow_block(block: &Block<ILOperand>) -> serde_json::Value {
    match block {
        Block::ConditionalBlock(block) => json!({
            "tag": "CONDITION",
            "condition": block.condition.iter().map(|i| i.print_syntax()).collect::<serde_json::Value>(),
            "trueBranch": block.true_branch.iter().map(|b| serialize_flow_block(b)).collect::<serde_json::Value>(),
            "falseBranch": block.false_branch.iter().map(|b| serialize_flow_block(b)).collect::<serde_json::Value>(),
        }),
        Block::DoWhileBlock(block) => json!({
            "tag": "DO_WHILE",
            "condition": block.condition.iter().map(|i| i.print_syntax()).collect::<serde_json::Value>(),
            "body": block.body.iter().map(|b| serialize_flow_block(b)).collect::<serde_json::Value>(),
        }),
        Block::ForBlock(block) => json!({
            "tag": "FOR",
            "condition": block.condition.iter().map(|i| i.print_syntax()).collect::<serde_json::Value>(),
            "modifier": block.modifier.iter().map(|i| i.print_syntax()).collect::<serde_json::Value>(),
            "body": block.body.iter().map(|b| serialize_flow_block(b)).collect::<serde_json::Value>(),
        }),
        Block::SequentialBlock(block) => json!({
            "tag": "SEQUENCE",
            "instructions": block.instructions.iter().map(|i| i.print_syntax()).collect::<serde_json::Value>(),
        }),
        Block::WhileBlock(block) => json!({
            "tag": "WHILE",
            "condition": block.condition.iter().map(|i| i.print_syntax()).collect::<serde_json::Value>(),
            "body": block.body.iter().map(|b| serialize_flow_block(b)).collect::<serde_json::Value>(),
        }),
    }
}

#[no_mangle]
pub fn decompile_il() {
    trace!("decompile_il");
    with_project_mut(&mut |project| {
        project.decompile_il();
    });
    trace!("decompile_il: done");
}

#[no_mangle]
pub fn analyze_control_flow() {
    trace!("analyze_control_flow");
    with_project_mut(&mut |project| {
        project.analyze_control_flow();
    });
    trace!("analyze_control_flow: done");
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
