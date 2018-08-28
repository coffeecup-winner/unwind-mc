#![feature(nll)]
#![feature(slice_patterns)]
extern crate either;
extern crate libudis86_sys;

pub type TODO = i32;

pub mod analyzer;
pub mod asm;
pub mod assignment_tracker;
pub mod common;
pub mod flow_analyzer;
pub mod il;
pub mod il_decompiler;
pub mod udis86;

#[cfg(test)]
mod tests {
    use analyzer::Analyzer;

    #[test]
    fn create_empty_analyzer() {
        let mut analyzer = match Analyzer::create(vec![90], 0) {
            Ok(analyzer) => analyzer,
            Err(_) => panic!("Failed to create an analyzer."),
        };
        analyzer.analyze();
    }
}
