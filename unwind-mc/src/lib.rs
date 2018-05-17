#![feature(nll)]
extern crate capstone;

pub type TODO = i32;

pub mod asm;
pub mod common;
pub mod il;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
