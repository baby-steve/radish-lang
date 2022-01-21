use std::fmt;

use crate::{
    compiler::{
        ast::*,
        table::{Symbol, SymbolTable},
        visitor::Visitor,
    },
};

#[derive(Debug)]
pub struct SemanticAnalyzer {
    pub scopes: Vec<SymbolTable>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer {
            scopes: vec![SymbolTable::new(0)],
        }
    }

    pub fn analyze(&mut self, ast: &AST) {
        for node in &ast.items {
            self.statement(&node);
        }
        
        println!("{}", self.scopes[self.scopes.len() - 1]);
    }

    fn enter_scope(&mut self) {
        self.scopes.push(SymbolTable::new(self.scopes.len()));
    }

    fn exit_scope(&mut self) {
        println!("{}", self.scopes[self.scopes.len() - 1]);
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
    fn block(&mut self, body: &Vec<Stmt>) {
        self.enter_scope();

        for node in body {
            self.statement(&node);
        }

        self.exit_scope();
    }

    fn var_declaration(&mut self, id: &Ident, init: &Option<Expr>) {
        if let Some(expr) = &init {
            self.expression(&expr);
        }

        match self.add_symbol(&id.name, Symbol { location: 0 }) {
            Some(old_value) => {
                if self.scopes.len() > 1 {
                    panic!(
                        "Identifier '{}' has already been declared in this scope. First declaration at {}", 
                        &id.name, old_value.location
                    );
                }
            }
            None => return,
        }
    }

    fn assignment(&mut self, id: &Ident, _: &OpAssignment, expr: &Expr) {
        self.expression(&expr);
        self.identifier(&id);
    }

    fn identifier(&mut self, id: &Ident) {
        if self.resolve_local(&id.name) == None {
            panic!("identifier '{}' not found.", &id.name);
        }
    }
}

impl fmt::Display for SemanticAnalyzer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for scope in &self.scopes {
            writeln!(f, "{}", scope)?;
        };

        Ok(())
    }
}