use regex::Regex;

use unwind_mc::analyzer::Analyzer;

pub fn analyze<'a>(func: &'a str) -> Analyzer {
    let normal_line_regex = Regex::new(
        r"^(?P<address>[0-9a-fA-F]{8}): (?P<bytes>((([0-9a-fA-F]{2})|  ) ){6}) (?P<asm>.+)$",
    ).unwrap();
    let bytes_regex = Regex::new(r"(?P<byte>[0-9a-fA-F]{2}) ?").unwrap();

    let lines: Vec<&str> = func
        .split(|c| c == '\r' || c == '\n')
        .filter(|l| match *l {
            "" => false,
            _ => true,
        }).map(|l| l.trim())
        .collect();

    let mut bytes: Vec<u8> = vec![];

    let groups = normal_line_regex.captures(lines[0]).unwrap();
    let address = u64::from_str_radix(&groups["address"], 16).unwrap();
    let mut canon_lines = String::new();
    for line in lines.iter() {
        match normal_line_regex.captures(line) {
            Some(capture) => {
                if canon_lines != "" {
                    canon_lines.push('\n');
                }
                canon_lines.push_str(&format!("{} ", &capture["address"]));
                for byte in bytes_regex.captures_iter(&capture["bytes"]) {
                    let b = u8::from_str_radix(&byte["byte"], 16).unwrap();
                    bytes.push(b);
                    canon_lines.push_str(&format!("{:02x}", b));
                }
            }
            None => {
                for byte in bytes_regex.captures_iter(line) {
                    let b = u8::from_str_radix(&byte["byte"], 16).unwrap();
                    bytes.push(b);
                    canon_lines.push_str(&format!("{:02x}", b));
                }
            }
        }
    }
    canon_lines.push('\n');

    let mut analyzer = Analyzer::create(bytes, address).unwrap();
    analyzer.add_function(address);
    analyzer.analyze();

    let mut instructions = String::new();
    for (_, i) in analyzer.graph().instructions_iter() {
        instructions.push_str(&format!("{:08x} {}\n", i.address, i.hex));
    }

    assert_eq!(instructions, canon_lines);

    analyzer
}
