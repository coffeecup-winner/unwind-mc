#![feature(nll)]
extern crate libudis86_sys;

pub type TODO = i32;

pub mod analyzer;
pub mod asm;
pub mod assignment_tracker;
pub mod common;
pub mod il;
pub mod udis86;

#[cfg(test)]
mod tests {
    use analyzer::Analyzer;

    #[test]
    fn create_empty_analyzer() {
        let mut analyzer = match Analyzer::create(&[90], 0) {
            Ok(analyzer) => analyzer,
            Err(_) => panic!("Failed to create an analyzer."),
        };
        analyzer.analyze();
    }
}
