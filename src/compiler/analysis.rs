use std::{collections::HashMap, fmt};

use crate::{
    common::span::Span,
    compiler::{ast::*, visitor::Visitor},
};

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

#[derive(Debug, PartialEq, Copy, Clone, Eq, Hash)]
pub struct Symbol {
    pub location: usize,
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub symbols: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            symbols: HashMap::new(),
        }
    }

    pub fn add_symbol(&mut self, key: &str, value: Symbol) -> Option<Symbol> {
        self.symbols.insert(key.to_string(), value)
    }
}

pub struct SemanticAnalyzer {
    pub scopes: Vec<SymbolTable>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer {
            scopes: vec![SymbolTable::new()],
        }
    }

    pub fn analyze(&mut self, ast: &AST) {
        for node in &ast.items {
            self.visit(&node)
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(SymbolTable::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn add_symbol(&mut self, name: &str, sym: Symbol) -> Option<Symbol> {
        let last = self.scopes.len() - 1;
        self.scopes[last].add_symbol(name, sym)
    }

    fn resolve_local(&mut self, name: &str) -> Option<&Symbol> {
        let mut depth = self.scopes.len();

        while depth > 0 {
            let scope = &self.scopes[depth - 1];
            depth -= 1;

            match scope.symbols.get(name) {
                Some(val) => return Some(val),
                None => continue,
            }
        }

        None
    }
}

impl Visitor for SemanticAnalyzer {
    fn block(&mut self, block: &BlockStmt) {
        self.enter_scope();
        for node in &block.body {
            self.visit(node);
        }

        self.exit_scope();
    }

    fn var_declaration(&mut self, decl: &VarDeclaration) {
        if let Some(expr) = &decl.init {
            self.visit(&expr);
        }

        match self.add_symbol(&decl.id.name, Symbol { location: 0 }) {
            Some(old_value) => {
                if self.scopes.len() > 1 {
                    panic!(
                        "Identifier '{}' has already been declared in this scope. First declaration at {}", 
                        &decl.id.name, old_value.location
                    );
                }
            }
            None => return,
        }
    }

    fn assignment(&mut self, stmt: &Assignment) {
        self.visit(&stmt.expr);

        self.identifier(&stmt.id);
    }

    fn identifier(&mut self, id: &Ident) {
        if self.resolve_local(&id.name) == None {
            panic!("identifier '{}' not found.", &id.name);
        }
    }
}
