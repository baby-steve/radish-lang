use std::{collections::HashMap, fmt};

#[derive(Debug, PartialEq, Copy, Clone, Eq, Hash)]
pub struct Symbol {
    pub location: usize,
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<Symbol>")
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub symbols: HashMap<String, Symbol>,
    pub depth: usize,
}

impl SymbolTable {
    pub fn new(depth: usize) -> Self {
        SymbolTable {
            symbols: HashMap::new(),
            depth,
        }
    }

    pub fn add_symbol(&mut self, key: &str, value: Symbol) -> Option<Symbol> {
        self.symbols.insert(key.to_string(), value)
    }
}

impl fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut entries = vec![];

        let mut max_width = 12;

        for value in self.symbols.iter() {
            if value.0.len() > max_width {
                max_width = value.0.len();
            }

            entries.push(value);
        }

        writeln!(f, "  Depth: {}", self.depth)?;
        writeln!(f, "╭─{}─┬{}╮", "─".repeat(max_width), "─".repeat(10))?;

        writeln!(f, "│ name{} │ {:<8} │", " ".repeat(max_width - 4), "type")?;

        let seperator = format!("├─{}─┼{}┤", "─".repeat(max_width), "─".repeat(10));

        for entry in entries {
            let (key, value) = entry;
            let padding = " ".repeat(max_width - key.len());
            writeln!(f, "{}", seperator)?;
            writeln!(f, "│ {}{} │ {:>12} │", key, padding, value)?;
        }

        writeln!(f, "╰─{}─┴{}╯", "─".repeat(max_width), "─".repeat(10))?;

        Ok(())
    }
}
