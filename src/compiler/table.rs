use std::{collections::HashMap, fmt};
use crate::common::span::Span;

#[derive(Debug, PartialEq, Copy, Clone, Eq, Hash)]
pub enum SymbolKind {
    Var,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Symbol(SymbolKind, pub Span);

impl Symbol {
    pub fn new(kind: SymbolKind, span: &Span) -> Symbol {
        Symbol(kind, Span::from(&span))
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Symbol<type=\"{:?}\", pos={:?}>", self.0, self.1)
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    symbols: HashMap<String,Symbol>,
    depth: usize,
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

    pub fn get_symbol(&self, key: &str) -> Option<&Symbol> {
        self.symbols.get(key)
    }
}

impl fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut entries = vec![];
        let l_width = 40;
        let mut max_width = 12;

        for value in self.symbols.iter() {
            if value.0.len() > max_width {
                max_width = value.0.len();
            }

            entries.push(value);
        }

        writeln!(f, "  Depth: {}", self.depth)?;
        writeln!(f, "╭─{}─┬{}╮", "─".repeat(max_width), "─".repeat(l_width))?;

        writeln!(f, "│ name{} │ {:<38} │", " ".repeat(max_width - 4), "type")?;

        let seperator = format!("├─{}─┼{}┤", "─".repeat(max_width), "─".repeat(l_width));

        for entry in entries {
            let (key, value) = entry;
            let padding = " ".repeat(max_width - key.len());
            let l_padding = " ".repeat(l_width - 2 - value.to_string().len());
            writeln!(f, "{}", seperator)?;
            writeln!(f, "│ {}{} │ {}{} │", key, padding, value, l_padding)?;
        }

        writeln!(f, "╰─{}─┴{}╯", "─".repeat(max_width), "─".repeat(l_width))?;

        Ok(())
    }
}
