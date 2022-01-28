use std::collections::HashSet;
use std::fmt;

use crate::compiler::{
    ast::*,
    table::{Symbol, SymbolKind, SymbolTable},
    visitor::Visitor,
};

#[derive(Debug)]
pub struct Analyzer {
    /// Chain of enclosing scopes.
    pub scopes: Vec<SymbolTable>,
    /// Keep track of variables that where referenced before assignment
    pub unresolved: HashSet<String>,
    /// Flag set to true if the analyzer is currently in a loop.
    in_loop: bool,
}

impl Analyzer {
    pub fn new() -> Self {
        Analyzer {
            scopes: vec![SymbolTable::new(0)],
            unresolved: HashSet::new(),
            in_loop: false,
        }
    }

    pub fn analyze(&mut self, ast: &AST) {
        self.register_functions(&ast);

        println!("{}", self.scopes[self.scopes.len() - 1]);

        for node in &ast.items {
            self.statement(&node);
        }

        println!("{}", self.scopes[self.scopes.len() - 1]);

        if !self.unresolved.is_empty() {
            for err in self.unresolved.iter() {
                println!("found undefined variable '{}'.", err);
            }

            panic!("Aborting due to the incorrectness of the given program");
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(SymbolTable::new(self.scopes.len()));
    }

    fn exit_scope(&mut self) {
        println!("{}", self.scopes[self.scopes.len() - 1]);
        self.scopes.pop();
    }

    /// Add a symbol to the current scope. If the symbol is already in
    /// this scope returns an error (for now just panics).
    fn add_symbol(&mut self, name: &str, sym: Symbol) -> Result<(), String> {
        let last = self.scopes.len() - 1;

        match self.scopes[last].add_symbol(name, sym) {
            Some(old_value) => {
                // Todo: this should return an Analysis error.
                panic!(
                    "Identifier '{}' has already been declared in this scope. first declaration at {:?}",
                    &name, old_value.1,
                );
            }
            None => Ok(()),
        }
    }

    /// Resolve a symbol by recursively checking each enclosing scope.
    /// If the symbol isn't found then returns None.
    fn try_resolve(&mut self, name: &str) -> Option<&Symbol> {
        let mut depth = self.scopes.len();

        while depth > 0 {
            let scope = &self.scopes[depth - 1];
            depth -= 1;

            match scope.get_symbol(name) {
                Some(val) => return Some(val),
                None => continue,
            }
        }

        None
    }

    fn resolve_symbol(&mut self, name: &str) -> Option<&Symbol> {
        if let Some(symbol) = self.try_resolve(name) {
            return Some(symbol);
        }

        None
    }

    // Todo: give this a better name.
    // Note: should this be done by the parser?
    /// Add all functions declared in the global scope to
    /// the current symbol table.
    fn register_functions(&mut self, ast: &AST) {
        for node in &ast.items {
            match node {
                Stmt::FunDeclaration(fun, _) => {
                    self.add_symbol(&fun.id.name, Symbol::from(fun)).unwrap();
                }
                _ => continue,
            }
        }
    }
}

impl Visitor for Analyzer {
    fn block(&mut self, body: &Vec<Stmt>) {
        self.enter_scope();

        for node in body {
            self.statement(&node);
        }

        self.exit_scope();
    }

    fn function_declaration(&mut self, fun: &Function) {
        if self.scopes.len() > 1 {
            self.add_symbol(&fun.id.name, Symbol::from(fun)).unwrap();
        }

        self.enter_scope();

        for param in &fun.params {
            self.add_symbol(&param.name, Symbol::new(SymbolKind::Var, &param.pos)).unwrap();
        }

        for node in fun.body.iter() {
            self.statement(&node);
        }

        self.exit_scope();
    }

    fn var_declaration(&mut self, id: &Ident, init: &Option<Expr>) {
        if let Some(expr) = &init {
            self.expression(&expr);
        }

        if self.scopes.len() == 1 {
            match self.unresolved.get(&id.name) {
                Some(_) => {
                    self.unresolved.remove(&id.name);
                }
                None => {}
            };
        }

        self.add_symbol(&id.name, Symbol::new(SymbolKind::Var, &id.pos))
            .unwrap();
    }

    fn assignment(&mut self, id: &Ident, _: &OpAssignment, expr: &Expr) {
        self.expression(&expr);
        self.identifier(&id);
    }

    fn call_expr(&mut self, callee: &Expr, args: &Vec<Box<Expr>>) {
        if !callee.is_callable() {
            panic!("Looks like you just tried to call something that wasn't callable.");
        }

        match callee {
            Expr::Identifier(id) => {
                if let Some(symbol) = self.resolve_symbol(&id.name) {
                    if let SymbolKind::Fun { arg_count } = symbol.0 {
                        if args.len() != arg_count {
                            panic!(
                                "The function '{}' takes {} argument{}, but got {}.",
                                &id.name,
                                &arg_count,
                                if arg_count == 1 { "" } else { "s" },
                                &args.len(),
                            )
                        }
                    }
                } else {
                    // if its the global scope, then its an error.
                    if self.scopes.len() == 1 {
                        panic!("Could not find '{}' anywhere.", &id.name);
                    }

                    // if we're in a local scope, it could be declared
                    // later in global scope.
                    self.unresolved.insert(id.name.clone());
                }
            }
            _ => unimplemented!("Can only call identifiers currently."),
        };

        for arg in args {
            self.expression(&arg);
        }
    }

    fn identifier(&mut self, id: &Ident) {
        if self.resolve_symbol(&id.name) == None {
            // if its the global scope, then its an error.
            if self.scopes.len() == 1 {
                panic!("Could not find '{}' anywhere.", &id.name);
            }

            // if we're in a local scope, it could be declared
            // later in global scope.
            self.unresolved.insert(id.name.clone());
        }
    }

    fn while_statement(&mut self, expr: &Expr, body: &Vec<Stmt>) {
        self.expression(&expr);

        self.in_loop = true;

        self.block(&body);

        self.in_loop = false;
    }

    fn loop_statement(&mut self, body: &Vec<Stmt>) {
        self.in_loop = true;

        self.block(&body);

        self.in_loop = false;
    }

    fn break_statement(&mut self) {
        // check if break is outside of a loop.
        if !self.in_loop {
            // Todo: make this an error, don't just panic.
            panic!("Break statement outside a loop");
        }
    }

    fn continue_statement(&mut self) {
        // check if continue is outside of a loop.
        if !self.in_loop {
            // Todo: make this an error, don't just panic.
            panic!("Continue statement outside a loop");
        }
    }
}

impl fmt::Display for Analyzer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for scope in &self.scopes {
            writeln!(f, "{}", scope)?;
        }

        Ok(())
    }
}
