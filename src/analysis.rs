use std::collections::HashSet;
use std::fmt;

use crate::{ast::*, span::Span, visitor::Visitor};

#[derive(Debug, PartialEq, Clone)]
pub struct SemanticError {
    pub error: String,
    pub span: Span,
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let message = format!("{}", self.error);

        write!(f, "Semantic Error: {}\n{}", &message, self.span)
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Symbol {
    VarSymbol,
}

pub struct SymbolTable {
    pub symbols: HashSet<Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            symbols: HashSet::new(),
        }
    }

    pub fn insert(&mut self, value: Symbol) -> bool {
        self.symbols.insert(value)
    }
}

pub struct SemanticAnalyzer {}

impl Visitor for SemanticAnalyzer {
    fn var_declaration(&mut self, decl: &VarDeclaration) {
        self.visit(&decl.init);
    }

    fn assignment(&mut self, stmt: &Assignment) {}

    fn identifier(&mut self, id: &Ident) {}
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer {}
    }

    pub fn analyze(&mut self, ast: &AST) {
        for node in &ast.items {
            self.visit(&node)
        }
    }
}
