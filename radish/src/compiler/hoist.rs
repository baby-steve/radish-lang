//! Module for the AST pass that handles variable scoping.
// Note: this entire file is essentially one giant bug filled hack and is
// in a desperate need of a rewrite. For your own sake, please don't examine
// its contents too closely as it may cause harm or even death.

use std::collections::HashMap;

use serde::Serialize;

use super::{
    ast_ast::{BlockStmt, ClassDecl, FunctionDecl, Identifier, MethodDecl, VariableDecl},
    visitor::VisitorResult,
    Stmt, SyntaxError, Visitor, AST,
};

pub fn hoist(ast: &mut AST) -> Result<(), SyntaxError> {
    let mut hoister = Hoister::new();

    hoister.analyze(ast)?;

    Ok(())
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum VarScope {
    Unknown,
    Global,
    Local(bool),
    NonLocal,
}

impl Default for VarScope {
    fn default() -> Self {
        VarScope::Unknown
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ScopeTyp {
    Unknown,
    Local(Local),
}

impl Default for ScopeTyp {
    fn default() -> Self {
        ScopeTyp::Unknown
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Local(String, usize, bool);

impl Local {
    pub fn is_captured(&self) -> bool {
        self.2
    }

    pub fn depth(&self) -> usize {
        self.1
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UpValue {
    pub index: usize,
    pub on_stack: bool,
}

impl UpValue {
    pub fn new(index: usize, on_stack: bool) -> Self {
        Self { index, on_stack }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    name: String,
    locals: Vec<Local>,
    // TODO: this is essentially just a counter. It can probably be
    // replaced with a usize or something.
    pub upvalues: Vec<UpValue>,
    pub alt_upvalues: Vec<(String, usize)>,
    pub upvalue_indexes: HashMap<String, usize>,
    depth: usize,
}

impl Scope {
    pub fn new(name: impl ToString) -> Self {
        Self {
            name: name.to_string(),
            locals: vec![],
            upvalues: vec![],
            alt_upvalues: vec![],
            upvalue_indexes: HashMap::new(),
            depth: 0,
        }
    }

    fn get_local(&self, name: &str) -> Option<(ScopeTyp, usize)> {
        let mut index = self.locals.len();

        while index > 0 {
            let local = &self.locals[index - 1];

            if local.0 == name {
                return Some((ScopeTyp::Local(local.clone()), index - 1));
            }

            index -= 1;
        }

        None
    }

    fn get_upvalue(&self, name: &str) -> Option<usize> {
        if let Some(index) = self.upvalue_indexes.get(name) {
            Some(*index)
        } else {
            None
        }
    }

    /// find the local with the given name and capture it.
    fn capture_local(&mut self, name: &str) {
        for mut local in self.locals.iter_mut().rev() {
            if local.0 == name {
                local.2 = true;
                break;
            }
        }
    }

    fn add_local(&mut self, name: impl ToString) {
        let local = Local(name.to_string(), self.depth, false);

        self.locals.push(local);
    }

    fn add_upvalue(&mut self, name: impl ToString, index: usize, on_stack: bool) {
        let array_pos = self.upvalues.len();

        let upval = UpValue::new(index, on_stack);

        self.upvalues.push(upval);

        self.upvalue_indexes.insert(name.to_string(), array_pos);
    }

    fn has_local(&self, name: &str) -> bool {
        for local in self.locals.iter() {
            if local.0 == name {
                //println!("[hoist] this scope has {}", name);
                return true;
            }
        }

        //println!("[hoist] this scope does not have {}", name);
        false
    }

    fn has_upvalue(&self, name: &str) -> bool {
        self.upvalue_indexes.contains_key(name)
    }

    fn enter_block(&mut self) {
        self.depth += 1;
        //println!("[hoist] entering block with depth of {}", self.depth);
    }

    fn exit_block(&mut self) -> Vec<Local> {
        //println!("[hoist] exiting block with depth of {}", self.depth);
        self.depth -= 1;

        let mut locals = vec![];

        while !self.locals.is_empty() && self.locals[self.locals.len() - 1].depth() > self.depth {
            let _local = self.locals.pop().unwrap();
            //println!("[hoist] removing \"{}\" from block", _local.0);

            locals.push(_local);
        }

        locals
    }
}

#[derive(Debug, PartialEq)]
pub struct Hoister {
    scopes: Vec<Scope>,
    captures: Vec<String>,
}

impl Hoister {
    fn new() -> Self {
        Self {
            scopes: vec![],
            captures: vec![],
        }
    }

    pub fn analyze(&mut self, ast: &mut AST) -> Result<(), SyntaxError> {
        self.enter_scope("global");

        for node in ast.items.iter_mut() {
            match self.visit_stmt(node) {
                Ok(_) => continue,
                Err(err) => return Err(err),
            }
        }

        let scope = self.exit_scope();

        ast.other_scope = Some(scope);

        Ok(())
    }

    fn scope(&self) -> &Scope {
        let last = self.scopes.len() - 1;
        &self.scopes[last]
    }

    fn scope_mut(&mut self) -> &mut Scope {
        let last = self.scopes.len() - 1;
        &mut self.scopes[last]
    }

    fn enter_scope(&mut self, name: impl ToString) {
        //println!("[hoist] entering scope \"{}\"", &name.to_string());
        let new_scope = Scope::new(name);
        self.scopes.push(new_scope);
    }

    fn exit_scope(&mut self) -> Scope {
        if let Some(scope) = self.scopes.pop() {
            //println!("[hoist] exiting scope \"{}\"", scope.name);
            //println!("[hoist] now in scope \"{}\"", self.scope().name);
            return scope;
        } else {
            panic!("Tried to exit global scope");
        }
    }

    fn in_global_scope(&self) -> bool {
        if self.scopes.len() == 1 && self.scope().depth == 0 {
            true
        } else {
            false
        }
    }

    fn declare_local(&mut self, id: &mut Identifier) {
        if self.in_global_scope() {
            return;
        }

        //println!(
        //    "[hoist] adding local \"{}\" to scope with depth of {}",
        //    &id.name,
        //    self.scope().depth
        //);

        self.scope_mut().add_local(&id.name);

        // FIXME: currently adding a local to the current scope and then getting it
        // immediately afterwards which isn't great.
        let local = self.scope().get_local(&id.name).unwrap();

        id.index = local.1 as u32;
        id.typ = VarScope::Local(false);
    }

    fn resolve_ident(&mut self, id: &mut Identifier) {
        if self.in_global_scope() {
            return;
        }

        // TODO: the following two if statements could use the `get_variable` method and
        // be combined into one statement.
        if self.scope().has_local(&id.name) {
            let (_, index) = self.scope().get_local(&id.name).unwrap();

            id.index = index as u32;
            id.typ = VarScope::Local(false);

            return;
        }

        if self.scope().has_upvalue(&id.name) {
            let index = self.scope().get_upvalue(&id.name).unwrap();

            id.index = index as u32;
            id.typ = VarScope::NonLocal;
            return;
        }

        self.resolve_upvalue(id);

        //if let Some((depth, index)) = self.resolve_upvalue(id) {
        //    self.handle_resolved_upvalue(id, index, depth);
        //}
    }

    fn resolve_upvalue(&mut self, id: &mut Identifier) {
        // exit the current scope
        let mut prev_scopes = vec![self.exit_scope()];

        let mut found = None;

        // check each enclosing scope till we find the one where the local was declared in
        // or one that's already captured the identifier as an upvalue.
        while let Some(mut scope) = self.scopes.pop() {
            if let Some(_local) = scope.get_local(&id.name) {
                // This is the scope the local was declared in.
                scope.capture_local(&id.name);

                // get the local's index.
                let index = scope.get_local(&id.name).unwrap().1;

                found = Some(index);

                // renter the scope
                self.scopes.push(scope);

                break;
            }
            /*else if let Some(upval) = scope.get_upvalue_2(&id.name) {
                // This scope has already captured an identifier with the same name.
                found = Some(upval.index);

                break;
            }*/
            else if let Some(upval) = scope.upvalue_indexes.get(&id.name) {
                found = Some(*upval);
                break;
            } else {
                // store the scope so that we can renter it later.
                prev_scopes.push(scope);
            }
        }

        let index = if let Some(i) = found {
            i
        } else {
            while let Some(scope) = prev_scopes.pop() {
                self.scopes.push(scope);
            }

            return;
        };

        let mut scope = prev_scopes.pop().unwrap();

        scope.add_upvalue(&id.name, index, true);

        self.scopes.push(scope);

        while let Some(mut scope) = prev_scopes.pop() {
            let index = self.scope().upvalue_indexes.get(&id.name).unwrap();

            scope.add_upvalue(&id.name, *index, false);
            self.scopes.push(scope);
        }

        id.index = self.scope_mut().upvalues.len() as u32;
        id.typ = VarScope::NonLocal;
    }

    fn _handle_resolved_upvalue(&mut self, id: &mut Identifier, pos: usize, depth: usize) {
        let _on_stack = if depth == 0 { false } else { true };

        let index = self.scope_mut().upvalues.len();

        self.scope_mut().add_upvalue(&id.name, pos, true);

        self.scope_mut()
            .alt_upvalues
            .push((id.name.clone(), usize::MAX));

        id.index = index as u32;
        id.typ = VarScope::NonLocal;
    }

    fn capture_locals(&mut self, locals: Vec<Local>, block: &mut Vec<Stmt>) {
        // this whole function is really gnarly.

        let mut captures = vec![];

        for stmt in block.iter_mut() {
            match stmt {
                Stmt::VariableDecl(stmt) => {
                    for local in locals.iter() {
                        if local.is_captured() && stmt.id.name == local.0 {
                            captures.push(local);
                            break;
                        }
                    }
                }
                Stmt::FunctionDecl(fun) => {
                    for local in locals.iter() {
                        if local.is_captured() && fun.id.name == local.0 {
                            captures.push(local);
                            break;
                        }
                    }
                }
                _ => continue,
            }
        }

        for upval in self.scope_mut().alt_upvalues.iter_mut() {
            for (i, cap) in captures.iter().enumerate() {
                if cap.0 == upval.0 {
                    upval.1 = i;

                    break;
                }
            }
        }
    }
}

impl Visitor<'_> for Hoister {
    fn visit_class_decl(&mut self, class: &mut ClassDecl) -> VisitorResult {
        for stmt in class.body.iter_mut() {
            self.visit_stmt(stmt)?;
        }

        Ok(())
    }

    fn visit_method_decl(&mut self, method: &mut MethodDecl) -> VisitorResult {
        self.enter_scope(&method.body.id.name);

        self.scope_mut().enter_block();

        for param in method.body.params.iter_mut() {
            self.declare_local(param);
        }

        self.scope_mut().add_local("this");

        self.visit_block_stmt(&mut method.body.body)?;

        let locals = self.scope_mut().exit_block();

        for param in method.body.params.iter_mut() {
            for local in locals.iter() {
                if local.is_captured() && param.name == local.0 {
                    break;
                }
            }
        }

        self.capture_locals(locals, &mut method.body.body.body);
        // don't call exit scope until `capture_locals` has been called.
        let scope = self.exit_scope();

        method.body.scope = Some(scope);

        Ok(())
    }

    fn visit_fun_decl(&mut self, fun: &mut FunctionDecl) -> VisitorResult {
        self.declare_local(&mut fun.id);

        self.enter_scope(&fun.id.name);

        self.scope_mut().enter_block();

        for param in fun.params.iter_mut() {
            self.declare_local(param);
        }

        // self.visit_block_stmt(&mut fun.body)?;

        for stmt in fun.body.body.iter_mut() {
            self.visit_stmt(stmt)?;
        }

        let locals = self.scope_mut().exit_block();

        for param in fun.params.iter_mut() {
            for local in locals.iter() {
                if local.is_captured() && param.name == local.0 {
                    break;
                }
            }
        }

        self.capture_locals(locals, &mut fun.body.body);
        // don't call exit scope until `capture_locals` has been called.
        let scope = self.exit_scope();

        fun.scope = Some(scope);

        Ok(())
    }

    fn visit_block_stmt(&mut self, block_stmt: &mut BlockStmt) -> VisitorResult {
        self.scope_mut().enter_block();

        for stmt in block_stmt.body.iter_mut() {
            self.visit_stmt(stmt)?;
        }

        let locals = self.scope_mut().exit_block();

        self.capture_locals(locals, &mut block_stmt.body);

        Ok(())
    }

    fn visit_var_decl(&mut self, stmt: &mut VariableDecl) -> VisitorResult {
        if let Some(expr) = &mut stmt.init {
            self.visit_expr(expr)?;
        }

        self.declare_local(&mut stmt.id);

        Ok(())
    }

    fn visit_ident(&mut self, ident: &mut Identifier) -> VisitorResult {
        self.resolve_ident(ident);

        Ok(())
    }
}
