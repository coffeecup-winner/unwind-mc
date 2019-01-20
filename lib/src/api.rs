use std;

const VERSION: &str = "0.1.0";

#[no_mangle]
pub extern "C" fn version() -> &'static str {
    return VERSION;
}
