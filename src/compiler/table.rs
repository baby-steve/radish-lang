use crate::common::span::Span;
use crate::compiler::ast::Function;
use std::{collections::HashMap, fmt};

#[derive(Debug, PartialEq, Copy, Clone, Eq, Hash)]
pub enum SymbolKind {
    Var,
    Fun { arg_count: usize },
}

#[derive(Debug, PartialEq, Clone)]
pub struct Symbol(pub SymbolKind, pub Span);

impl Symbol {
    pub fn new(kind: SymbolKind, span: &Span) -> Symbol {
        Symbol(kind, Span::from(&span))
    }
}

impl From<&Function> for Symbol {
    fn from(fun: &Function) -> Symbol {
        let kind = SymbolKind::Fun {
            arg_count: fun.params.len(),
        };
        Symbol::new(kind, &fun.id.pos)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let contents = &self.1.source.contents;
        let start = self.1.start;
        let (start_line, start_col) = Span::get_line_index(&contents, start).unwrap();
        write!(
            f,
            "Symbol<type=\"{:?}\", pos=({},{})>",
            self.0, start_line, start_col
        )
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    symbols: HashMap<String, Symbol>,
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
        let l_width = 60;
        let mut max_width = 12;

        for value in self.symbols.iter() {
            if value.0.len() > max_width {
                max_width = value.0.len();
            }

            entries.push(value);
        }

        writeln!(f, "  Depth: {}", self.depth)?;
        writeln!(f, "╭─{}─┬{}╮", "─".repeat(max_width), "─".repeat(l_width))?;

        writeln!(f, "│ name{} │ type{} │", " ".repeat(max_width - 4), " ".repeat(l_width - 6))?;

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
