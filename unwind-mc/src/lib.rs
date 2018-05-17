#![feature(nll)]
extern crate capstone;

pub type TODO = i32;

pub mod analyzer;
pub mod asm;
pub mod common;
pub mod il;

#[cfg(test)]
mod tests {
    use analyzer::Analyzer;

    #[test]
    fn create_empty_analyzer() {
        assert_eq!(2 + 2, 4);
        match Analyzer::create(&[90], 0) {
            Ok(_) => {}
            Err(_) => assert!(false),
        }
    }
}
