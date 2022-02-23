use crate::common::span::Span;
use crate::compiler::ast::{Function, ClassDeclaration};
use std::{collections::HashMap, fmt};

#[derive(Debug, PartialEq, Copy, Clone, Eq, Hash)]
pub enum SymbolKind {
    Var,
    Fun { arg_count: usize },
    Class,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Symbol(pub SymbolKind, pub Span, pub usize);

impl Symbol {
    pub fn new(kind: SymbolKind, span: &Span, depth: usize) -> Symbol {
        Symbol(kind, Span::from(&span), depth)
    }

    pub fn var(span: &Span, depth: usize) -> Symbol {
        Symbol(SymbolKind::Var, Span::from(&span), depth)
    }

    pub fn fun(arg_count: usize, span: &Span, depth: usize) -> Symbol {
        Symbol(SymbolKind::Fun { arg_count }, Span::from(&span), depth)
    }

    pub fn class(span: &Span, depth: usize) -> Symbol {
        Symbol(SymbolKind::Class, Span::from(&span), depth)
    }
}

impl From<&Function> for Symbol {
    fn from(fun: &Function) -> Symbol {
        let kind = SymbolKind::Fun {
            arg_count: fun.params.len(),
        };
        Symbol::new(kind, &fun.id.pos, 0)
    }
}

impl From<&ClassDeclaration> for Symbol {
    fn from(fun: &ClassDeclaration) -> Symbol {
        let kind = SymbolKind::Class;
        Symbol::new(kind, &fun.id.pos, 0)
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

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolTable {
    pub locals: HashMap<String, Symbol>,
    pub non_locals: HashMap<String, Symbol>,
    // TODO: do we really need this?
    pub depth: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            locals: HashMap::new(),
            non_locals: HashMap::new(),
            depth: 0,
        }
    }

    pub fn with_depth(depth: usize) -> Self {
        SymbolTable {
            locals: HashMap::new(),
            non_locals: HashMap::new(),
            depth,
        }
    }

    pub fn add_local(&mut self, key: &str, value: Symbol) -> Option<Symbol> {
        self.locals.insert(key.to_string(), value)
    }

    pub fn get_local(&self, key: &str) -> Option<&Symbol> {
        self.locals.get(key)
    }

    pub fn add_non_local(&mut self, key: &str, value: Symbol) -> Option<Symbol> {
        self.non_locals.insert(key.to_string(), value)
    }

    pub fn get_non_local(&self, key: &str) -> Option<&Symbol> {
        self.non_locals.get(key)
    }

    pub fn has_local(&self, key: &str) -> bool {
        self.locals.contains_key(key)
    }

    pub fn has_non_local(&self, key: &str) -> bool {
        self.non_locals.contains_key(key)
    }

    pub fn all(&self) -> &HashMap<String, Symbol> {
        &self.locals
    }
}

impl fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut entries = vec![];
        let l_width = 60;
        let mut max_width = 12;

        for value in self.locals.iter() {
            if value.0.len() > max_width {
                max_width = value.0.len();
            }

            entries.push(value);
        }

        for value in self.non_locals.iter() {
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
