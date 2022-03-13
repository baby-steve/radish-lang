// TODO: split this into (at least) two seperate passes, one to validate the ast,
// the other to handle name resolution and hoisting.

use crate::common::Span;
use std::collections::HashSet;
use std::fmt;

use crate::compiler::{
    ast::*,
    error::{SyntaxError, SyntaxErrorKind},
    scope::{ScopeMap, Symbol, SymbolKind},
    visitor::{Visitor, VisitorResult},
};

use crate::error::Item;

pub fn resolve_symbols(ast: &mut AST) -> Result<(), SyntaxError> {
    let mut analyzer = Analyzer::new();

    analyzer.analyze(ast)
}

#[derive(Debug, Clone)]
pub struct Analyzer {
    /// Chain of enclosing scopes.
    pub scopes: Vec<ScopeMap>,
    /// Keep track of variables that where referenced before assignment
    pub unresolved: HashSet<Ident>,
}

impl Analyzer {
    pub fn new() -> Self {
        Analyzer::with_scope(ScopeMap::new())
    }

    pub fn with_scope(scope: ScopeMap) -> Analyzer {
        Analyzer {
            scopes: vec![scope],
            unresolved: HashSet::new(),
        }
    }

    pub fn analyze(&mut self, ast: &mut AST) -> Result<(), SyntaxError> {
        self.foward_declare(&ast);

        for node in ast.items.iter_mut() {
            match self.visit_stmt(node) {
                Ok(_) => continue,
                // TODO: handle errors.
                Err(err) => return Err(err),
            }
        }

        if !self.unresolved.is_empty() {
            for err in self.unresolved.iter() {
                self.unresolved_err(&err.name, &err.pos);
                println!("{:?}", err);
            }

            let err_kind = SyntaxErrorKind::Custom {
                message: "aborting due to previous errors".to_string(),
            };

            return Err(SyntaxError::new(err_kind));
        }

        let global_scope = self.scopes.pop().expect("missing global scope");

        ast.scope.extend(global_scope);

        Ok(())
    }

    /// Enter a new scope.
    fn enter_scope(&mut self) {
        self.scopes.push(ScopeMap::with_depth(self.scopes.len()));
    }

    /// Reenter an old scope. Used when resolving symbols.
    fn renter_scope(&mut self, scope: ScopeMap) {
        self.scopes.push(scope);
    }

    /// Exit the current scope. If its the last scope, returns `None`.
    fn exit_scope(&mut self) -> Option<ScopeMap> {
        //println!("{}", self.scopes[self.scopes.len() - 1]);

        if self.scopes.len() == 1 {
            return None;
        }
        self.scopes.pop()
    }

    /// Returns the topmost, i.e. local, scope, mutably.
    fn local_scope(&mut self) -> &mut ScopeMap {
        let last = self.scopes.len() - 1;
        &mut self.scopes[last]
    }

    /// Returns the topmost scope immutably.
    fn borrow_local_scope(&self) -> &ScopeMap {
        let last = self.scopes.len() - 1;
        &self.scopes[last]
    }

    /// Check the current scope for a local symbol.
    fn local_symbol(&self, name: &str) -> Option<Symbol> {
        for (local_name, symbol) in self.borrow_local_scope().locals.iter() {
            if local_name == name {
                return Some(symbol.clone());
            }
        }

        None
    }

    /// Check the current scope for a non local symbol, e.i. a symbol declared
    /// in an enclosing local scope but not the global scope.
    fn nonlocal_symbol(&self, name: &str) -> Option<Symbol> {
        for (non_local_name, symbol) in self.borrow_local_scope().non_locals.iter() {
            if non_local_name == name {
                return Some(symbol.clone());
            }
        }

        None
    }

    fn resolve_symbol(&mut self, name: &str) -> Option<Symbol> {
        // first check if its in the local scope.
        if let Some(symbol) = self.local_symbol(name) {
            return Some(symbol);
        }

        // check if its already in the current scope as a non local.
        if let Some(symbol) = self.nonlocal_symbol(name) {
            return Some(symbol);
        }

        // recursively check each enclosing scope.
        if let Some(scope) = self.exit_scope() {
            let resolved = self.resolve_symbol(name);
            self.renter_scope(scope);
            if let Some(symbol) = resolved {
                // yeah! we found it.
                // if its not in the global scope, but rather an enclosing scope,
                // then add it to this scope's nonlocals.
                if symbol.2 > 0 {
                    self.local_scope().add_non_local(name, symbol.clone());
                }
                return Some(symbol);
            }
        }

        // couldn't find it
        None
    }

    fn resolve_member_expression(&mut self, object: &mut Expr) -> Result<(), SyntaxError> {
        // Because the fields, methods, and constructors on a Class cannot be
        // know at compile time, we can't really resolve them. We can however
        // resolve the base expression. given something along the lines of: `a.b.c` we
        // can ensure that `a` is in the current scope.

        match object {
            Expr::Identifier(id) => {
                self.visit_ident(id)?;
            }
            Expr::MemberExpr(object, _, _) => {
                self.resolve_member_expression(object)?;
            }
            _ => unimplemented!(),
        };

        Ok(())
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
                Stmt::ClassDeclaration(class, _) => {
                    self.local_scope()
                        .add_local(&class.id.name, Symbol::from(class));
                }
                _ => continue,
            }
        }
    }

    /// Add a variable to the current scope, checking for duplicates.
    fn declare_variable(
        &mut self,
        name: &str,
        kind: SymbolKind,
        span: &Span,
    ) -> Result<(), SyntaxError> {
        let depth = self.scopes.len() - 1;

        let symbol = Symbol::new(kind, &span, depth);

        if let Some(Symbol(_, prev_pos, _)) = self.local_scope().add_local(&name, symbol) {
            return Err(self.duplicate_ids(&name, &span, &prev_pos));
        }

        Ok(())
    }

    /// Create an unresolved name error.
    fn unresolved_err(&self, name: &str, span: &Span) -> SyntaxError {
        let err_kind = SyntaxErrorKind::UnresolvedIdent {
            item: Item::new(span, name),
        };

        let err = SyntaxError::new(err_kind);

        err
    }

    /// Create a duplicate identifier error.
    fn duplicate_ids(&self, name: &str, pos: &Span, prev: &Span) -> SyntaxError {
        let err_kind = SyntaxErrorKind::DuplicateIdent {
            first: Item::new(prev, name),
            second: Item::new(pos, name),
        };

        let err = SyntaxError::new(err_kind);

        err
    }

    fn param_list(&mut self, params: &Vec<Ident>) -> Result<(), SyntaxError> {
        let depth = self.scopes.len() - 1;

        for param in params {
            if let Some(_) = self
                .local_scope()
                .add_local(&param.name, Symbol::var(&param.pos, depth))
            {
                let err_kind = SyntaxErrorKind::DuplicateParam {
                    param: Item::new(&param.pos, &param.name),
                };

                let err = SyntaxError::new(err_kind);

                return Err(err);
            }
        }

        Ok(())
    }
}

impl Visitor<'_> for Analyzer {
    fn visit_fun_decl(&mut self, fun: &mut FunctionDecl) -> VisitorResult {
        // Since all global functions are foward delcared, we
        // don't have to declare them here if we're in the global scope.
        if self.scopes.len() > 1 {
            self.declare_variable(
                &fun.id.name,
                SymbolKind::Fun {
                    arg_count: fun.params.len(),
                },
                &fun.id.pos,
            )?;
        }

        self.enter_scope();

        self.param_list(&fun.params)?;

        for node in fun.body.iter_mut() {
            self.visit_stmt(node)?;
        }

        let scope = self.exit_scope().unwrap();
        // We extend (create really) the function's scope here so that the compiler
        // can use it to handle upvalues (the nonlocals in this scope) and maybe locals.
        fun.scope.extend(scope);

        Ok(())
    }

    fn visit_class_decl(&mut self, class: &mut ClassDecl) -> VisitorResult {
        // Since all global classes are foward delcared, we
        // don't have to declare them here if we're in the global scope.
        if self.scopes.len() > 1 {
            self.declare_variable(&class.id.name, SymbolKind::Class, &class.id.pos)?;
        }

        self.enter_scope();

        for constructor in class.constructors.iter_mut() {
            self.visit_con_decl(constructor)?;
        }

        self.exit_scope();

        Ok(())
    }

    fn visit_con_decl(&mut self, con: &mut ConstructorDecl) -> VisitorResult {
        self.declare_variable(&con.id.name, SymbolKind::Con, &con.id.pos)?;

        self.enter_scope();

        self.param_list(&con.params)?;

        for node in con.body.iter_mut() {
            self.visit_stmt(node)?;
        }

        self.exit_scope();

        Ok(())
    }

    fn visit_block_stmt(&mut self, block: &mut Vec<Stmt>) -> VisitorResult {
        self.enter_scope();

        for stmt in block.iter_mut() {
            self.visit_stmt(stmt)?;
        }

        self.exit_scope();

        Ok(())
    }

    fn visit_var_decl(
        &mut self,
        id: &mut Ident,
        expr: &mut Option<Expr>,
        _kind: VarKind,
    ) -> VisitorResult {
        // check the right hand expression if it exists.
        match expr {
            Some(expr) => {
                self.visit_expr(expr)?;
            }
            _ => {}
        }

        // check if this is the definition of a previously unresolved global variable.
        if self.scopes.len() == 1 {
            match self.unresolved.get(&id) {
                Some(_) => {
                    self.unresolved.remove(&id);
                }
                None => {}
            };
        }

        // add this variable to the current scope, checking for duplicates.
        self.declare_variable(&id.name, SymbolKind::Var, &id.pos)?;

        Ok(())
    }

    fn visit_ident(&mut self, ident: &mut Ident) -> VisitorResult {
        if self.resolve_symbol(&ident.name) == None {
            // if its inside the global scope, then its an error.
            if self.scopes.len() == 1 {
                return Err(self.unresolved_err(&ident.name, &ident.pos));
            }

            // if we're in a local scope, it could be declared later in
            // global scope, so we stash it in the unresolved identifier's map.
            self.unresolved.insert(ident.clone());
        }

        Ok(())
    }

    fn visit_member_expr(&mut self, obj: &mut Expr, _prop: &mut Expr) -> VisitorResult {
        self.resolve_member_expression(obj)
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
