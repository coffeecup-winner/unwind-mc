use regex::Regex;

use unwindmc::project::Project;

pub fn analyze<'a>(func: &'a str) -> Project {
    let normal_line_regex = Regex::new(
        r"^(?P<address>[0-9a-fA-F]{8}): (?P<bytes>((([0-9a-fA-F]{2})|  ) ){6}) (?P<asm>.+)$",
    )
    .unwrap();
    let normal_line_regex_gcc = Regex::new(
        r"^(?P<address>[0-9a-fA-F]+):\t(?P<bytes>((([0-9a-fA-F]{2})|  ) ){7})\t(?P<asm>.+)$",
    )
    .unwrap();
    let bytes_regex = Regex::new(r"(?P<byte>[0-9a-fA-F]{2}) ?").unwrap();

    let lines: Vec<&str> = func
        .split(|c| c == '\r' || c == '\n')
        .filter(|l| match *l {
            "" => false,
            _ => true,
        })
        .map(|l| l.trim())
        .collect();

    let mut bytes: Vec<u8> = vec![];

    let mut groups = normal_line_regex.captures(lines[0]);
    let line_regex = match groups {
        Some(_) => normal_line_regex,
        None => {
            groups = normal_line_regex_gcc.captures(lines[0]);
            normal_line_regex_gcc
        }
    };
    let address = u64::from_str_radix(&groups.unwrap()["address"], 16).unwrap();
    let mut canon_lines = String::new();
    for line in lines.iter() {
        match line_regex.captures(line) {
            Some(capture) => {
                if canon_lines != "" {
                    canon_lines.push('\n');
                }
                let address = u64::from_str_radix(&capture["address"], 16).unwrap();
                canon_lines.push_str(&format!("{:08x} ", address));
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

    let mut project = Project::from_text(bytes, address).unwrap();
    project.add_function(address);
    project.analyze_asm();

    let mut instructions = String::new();
    for (_, i) in project.graph().instructions_iter() {
        instructions.push_str(&format!(
            "{:08x} {}\n",
            i.address,
            project.graph().get_bytes_as_hex(i)
        ));
    }

    assert_eq!(instructions, canon_lines);

    project
}

pub fn strip_indent(text: &str) -> String {
    let lines = text.lines().collect::<Vec<&str>>();
    let indent = lines
        .iter()
        .skip(1)
        .map(|l| l.chars().take_while(|&c| c == ' ').count())
        .min()
        .unwrap();
    let mut result = String::new();
    result += lines[0];
    result += "\n";
    for i in 1..lines.len() {
        let (_, trimmed_line) = lines[i].split_at(indent);
        result += trimmed_line;
        result += "\n";
    }
    String::from(result.trim()) + "\n"
}
