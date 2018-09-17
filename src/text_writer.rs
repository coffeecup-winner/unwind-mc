use std::collections::HashMap;

#[cfg(windows)]
const NEWLINE: &str = "\r\n";
#[cfg(not(windows))]
const NEWLINE: &str = "\n";

pub struct TextWriter {
    text: String,
    indent_size: u32,
    indent_cache: HashMap<u32, String>,
    indent_level: u32,
    indent: String,
    indent_pending: bool,
}

impl TextWriter {
    pub fn new(indent_size: u32) -> TextWriter {
        let mut writer = TextWriter {
            text: String::new(),
            indent_size,
            indent_cache: HashMap::new(),
            indent_level: 0,
            indent: String::from(""),
            indent_pending: false,
        };
        writer.indent_cache.insert(0, String::from(""));
        writer
    }

    pub fn increase_indent(&mut self) {
        let new_level = self.indent_level + 1;
        match self.indent_cache.get(&new_level) {
            Some(indent) => {
                self.indent_level = new_level;
                self.indent = indent.clone(); // TODO: remove clone()
            }
            None => {
                let indent = (0..(new_level * self.indent_size))
                    .map(|_| " ")
                    .collect::<String>();
                self.indent_cache.insert(new_level, indent.clone()); // TODO: remove clone()
                self.indent_level = new_level;
                self.indent = indent;
            }
        }
    }

    pub fn decrease_indent(&mut self) {
        let new_level = self.indent_level - 1;
        self.indent_level = new_level;
        self.indent = self.indent_cache[&new_level].clone(); // TODO: remove clone()
    }

    pub fn write(&mut self, text: &str) {
        if self.indent_pending {
            self.text += &self.indent;
            self.indent_pending = false;
        }
        self.text += text;
    }

    pub fn write_newline(&mut self) {
        self.text += NEWLINE;
        self.indent_pending = true;
    }

    pub fn build_text(&self) -> String {
        self.text.clone()
    }
}
