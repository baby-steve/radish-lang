use crate::common::span::Span;
use std::collections::HashSet;
use std::fmt;

use crate::compiler::{
    ast::*,
    error::{SemanticError, SemanticErrorKind},
    table::{Symbol, SymbolTable},
    visitor::Visitor,
};

use crate::error::Item;

#[derive(Debug, Clone)]
pub struct Analyzer {
    /// Chain of enclosing scopes.
    pub scopes: Vec<SymbolTable>,
    /// Keep track of variables that where referenced before assignment
    pub unresolved: HashSet<Ident>,
    /// Flag set to true if the analyzer is currently in a loop.
    in_loop: bool,
    in_function: bool, // are these really necessary?
}

impl Analyzer {
    pub fn new() -> Self {
        Analyzer::with_scope(SymbolTable::new(0))
    }

    pub fn with_scope(scope: SymbolTable) -> Analyzer {
        Analyzer {
            scopes: vec![scope],
            unresolved: HashSet::new(),
            in_loop: false,
            in_function: false,
        }
    }

    pub fn analyze(&mut self, ast: &AST) -> Result<SymbolTable, ()> {
        self.foward_declare(&ast);

        //println!("{}", self.scopes[self.scopes.len() - 1]);

        for node in &ast.items {
            match self.statement(&node) {
                Ok(_) => continue,
                // TODO: handle errors.
                Err(_) => continue,
            }
        }

        println!("{}", self.scopes[self.scopes.len() - 1]);

        if !self.unresolved.is_empty() {
            for err in self.unresolved.iter() {
                self.unresolved_err(&err.name, &err.pos);
                println!("{:?}", err);
            }

            return Err(());
        }

        Ok(self.scopes.pop().unwrap())
    }

    fn enter_scope(&mut self) {
        self.scopes.push(SymbolTable::new(self.scopes.len()));
    }

    fn renter_scope(&mut self, scope: SymbolTable) {
        self.scopes.push(scope);
    }

    fn exit_scope(&mut self) -> Option<SymbolTable> {
        println!("{}", self.scopes[self.scopes.len() - 1]);
        if self.scopes.len() == 1 {
            return None;
        }
        self.scopes.pop()
    }

    /// Returns the topmost, i.e. local, scope, mutably.
    fn local_scope(&mut self) -> &mut SymbolTable {
        let last = self.scopes.len() - 1;
        &mut self.scopes[last]
    }

    /// Returns the topmost scope immutably.
    fn borrow_local_scope(&self) -> &SymbolTable {
        let last = self.scopes.len() - 1;
        &self.scopes[last]
    }

    fn local_symbol(&self, name: &str) -> Option<Symbol> {
        for (local_name, symbol) in self.borrow_local_scope().locals.iter() {
            if local_name == name {
                return Some(symbol.clone());
            }
        }

        None
    }

    fn nonlocal_symbol(&self, name: &str) -> Option<Symbol> {
        for (non_local_name, symbol) in self.borrow_local_scope().non_locals.iter() {
            if non_local_name == name {
                return Some(symbol.clone());
            }
        }

        None
    }

    fn resolve_symbol(&mut self, name: &str) -> Option<Symbol> {
        if let Some(symbol) = self.local_symbol(name) {
            return Some(symbol);
        }

        if let Some(symbol) = self.nonlocal_symbol(name) {
            return Some(symbol);
        }

        if let Some(scope) = self.exit_scope() {
            let resolved = self.resolve_symbol(name);
            self.renter_scope(scope);
            if let Some(symbol) = resolved {
                if symbol.2 > 0 {
                    self.local_scope().add_non_local(name, symbol.clone());
                }
                return Some(symbol);
            }
        }

        None
    }

    /// Add all functions declared in the global scope to
    /// the current symbol table.
    fn foward_declare(&mut self, ast: &AST) {
        for node in &ast.items {
            match node {
                Stmt::FunDeclaration(fun, _) => {
                    self.local_scope()
                        .add_local(&fun.id.name, Symbol::from(fun));
                }
                _ => continue,
            }
        }
    }

    fn unresolved_err(&self, name: &str, span: &Span) -> SemanticError {
        let err_kind = SemanticErrorKind::UnresolvedIdent {
            item: Item::new(span, name),
        };

        let err = SemanticError::new(err_kind);

        err
    }

    fn duplicate_ids(&self, name: &str, pos: &Span, prev: &Span) -> SemanticError {
        let err_kind = SemanticErrorKind::DuplicateIdent {
            first: Item::new(prev, name),
            second: Item::new(pos, name),
        };

        let err = SemanticError::new(err_kind);

        err
    }
}

impl Visitor<(), SemanticError> for Analyzer {
    fn block(&mut self, body: &Vec<Stmt>) -> Result<(), SemanticError> {
        self.enter_scope();

        for node in body {
            self.statement(&node)?;
        }

        self.exit_scope();

        Ok(())
    }

    fn function_declaration(&mut self, fun: &Function) -> Result<(), SemanticError> {
        let depth = self.scopes.len() - 1;

        if self.scopes.len() > 1 {
            if let Some(Symbol(_, prev_pos, _)) = self.local_scope().add_local(
                &fun.id.name,
                Symbol::fun(fun.params.len(), &fun.id.pos, depth),
            ) {
                return Err(self.duplicate_ids(&fun.id.name, &fun.id.pos, &prev_pos));
            }
        }

        self.enter_scope();

        for param in &fun.params {
            if let Some(_) = self
                .local_scope()
                .add_local(&param.name, Symbol::var(&param.pos, depth))
            {
                let err_kind = SemanticErrorKind::DuplicateParam {
                    param: Item::new(&param.pos, &param.name),
                };

                let err = SemanticError::new(err_kind);

                return Err(err);
            }
        }

        for node in fun.body.iter() {
            self.statement(&node)?;
        }

        self.exit_scope();

        Ok(())
    }

    fn var_declaration(&mut self, id: &Ident, init: &Option<Expr>) -> Result<(), SemanticError> {
        if let Some(expr) = &init {
            self.expression(&expr)?;
        }

        if self.scopes.len() == 1 {
            match self.unresolved.get(&id) {
                Some(_) => {
                    self.unresolved.remove(&id);
                }
                None => {}
            };
        }

        let depth = self.scopes.len() - 1;
        if let Some(Symbol(_, prev_pos, _)) =
            self.local_scope().add_local(&id.name, Symbol::var(&id.pos, depth))
        {
            return Err(self.duplicate_ids(&id.name, &id.pos, &prev_pos));
        }

        Ok(())
    }

    fn assignment(
        &mut self,
        id: &Ident,
        _: &OpAssignment,
        expr: &Expr,
    ) -> Result<(), SemanticError> {
        self.expression(&expr)?;
        self.identifier(&id)?;

        Ok(())
    }

    fn call_expr(&mut self, _: &Expr, args: &Vec<Box<Expr>>) -> Result<(), SemanticError> {
        /*match callee {
            Expr::Identifier(id) => {
                if let Some(symbol) = self.resolve_symbol(&id.name) {
                    if let (SymbolKind::Fun { arg_count }, _) = symbol {
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
                        let err = self.unresolved_err(&id.name, &id.pos);
                        return Err(err);
                    }

                    // if we're in a local scope, it could be declared
                    // later in the global scope.
                    self.unresolved.insert(id.clone());
                }
            }
            _ => unimplemented!("Can only call identifiers currently."),
        };*/

        for arg in args {
            self.expression(&arg)?;
        }

        Ok(())
    }

    fn identifier(&mut self, id: &Ident) -> Result<(), SemanticError> {
        if self.resolve_symbol(&id.name) == None {
            // if its the global scope, then its an error.
            if self.scopes.len() == 1 {
                return Err(self.unresolved_err(&id.name, &id.pos));
            }

            // if we're in a local scope, it could be declared
            // later in global scope.
            self.unresolved.insert(id.clone());
        }

        Ok(())
    }

    fn while_statement(&mut self, expr: &Expr, body: &Vec<Stmt>) -> Result<(), SemanticError> {
        self.expression(&expr)?;

        self.in_loop = true;

        self.block(&body)?;

        self.in_loop = false;

        Ok(())
    }

    fn loop_statement(&mut self, body: &Vec<Stmt>) -> Result<(), SemanticError> {
        self.in_loop = true;

        self.block(&body)?;

        self.in_loop = false;

        Ok(())
    }

    fn return_statement(&mut self, _: &Option<Expr>) -> Result<(), SemanticError> {
        Ok(())
    }

    fn break_statement(&mut self, pos: &Span) -> Result<(), SemanticError> {
        if !self.in_loop {
            let err_kind = SemanticErrorKind::BreakOutsideLoop {
                item: Item::new(pos, "break"),
            };

            let err = SemanticError::new(err_kind);

            Err(err)
        } else {
            Ok(())
        }
    }

    fn continue_statement(&mut self, pos: &Span) -> Result<(), SemanticError> {
        if !self.in_loop {
            let err_kind = SemanticErrorKind::ContinueOutsideLoop {
                item: Item::new(pos, "continue"),
            };

            let err = SemanticError::new(err_kind);

            Err(err)
        } else {
            Ok(())
        }
    }
    fn number(&mut self, _: &f64) -> Result<(), SemanticError> {
        Ok(())
    }

    fn string(&mut self, _: &str) -> Result<(), SemanticError> {
        Ok(())
    }

    fn boolean(&mut self, _: &bool) -> Result<(), SemanticError> {
        Ok(())
    }

    fn nil(&mut self) -> Result<(), SemanticError> {
        Ok(())
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
