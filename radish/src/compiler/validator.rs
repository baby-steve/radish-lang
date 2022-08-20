//! Module containig an AST validator. this pass is meant to catch things that fit
//! into the AST's data structures but are sematically invalid. However it only
//! handles simple checks, not more compilcated things like name resolution.
//! 

use crate::common::Span;

use crate::compiler::{
    ast::AST,
    error::{SyntaxError, SyntaxErrorKind},
    visitor::VisitorResult,
    Visitor,
};

use crate::error::Item;

use super::ast_ast::{
    BreakStmt, ContinueStmt, FunctionDecl, LoopStmt, Position, ReturnStmt, VariableDecl,
    VariableKind, WhileStmt,
};

pub fn validate_ast(ast: &mut AST) -> Result<(), SyntaxError> {
    AstValidator::validate_ast(ast)
}

// TODO: rename this.
#[derive(Debug, Copy, Clone, PartialEq)]
enum State {
    Function,
    Loop,
}

struct AstValidator {
    /// Track the AST validator's state.
    state: Vec<State>,
}

impl AstValidator {
    pub fn validate_ast(ast: &mut AST) -> Result<(), SyntaxError> {
        let mut validator = AstValidator { state: Vec::new() };

        for stmt in ast.items.iter_mut() {
            validator.visit_stmt(stmt)?;
        }

        Ok(())
    }
}

impl Visitor<'_> for AstValidator {
    /// Mark that we're in a function body.
    fn visit_fun_decl(&mut self, fun: &mut FunctionDecl) -> VisitorResult {
        self.state.push(State::Function);

        self.visit_block_stmt(&mut fun.body)?;

        self.state
            .pop()
            .expect("expected validator state not to be empty");

        Ok(())
    }

    /// Validate the variable's inital value. If it doesn't have an
    /// inital value and its a constant (final) variable declaration, then
    /// report an error.
    fn visit_var_decl(&mut self, stmt: &mut VariableDecl) -> VisitorResult {
        match &mut stmt.init {
            Some(expr) => self.visit_expr(expr),
            None if stmt.kind == VariableKind::Final => {
                let err_kind = SyntaxErrorKind::MissingConstInit {
                    item: Item::new(&stmt.id.position(), &stmt.id.name),
                };

                Err(SyntaxError::new(err_kind))
            }
            _ => Ok(()),
        }
    }

    /// Mark that we've entered a loop.
    fn visit_loop_stmt(&mut self, stmt: &mut LoopStmt) -> VisitorResult {
        self.state.push(State::Loop);

        self.visit_block_stmt(&mut stmt.body)?;

        self.state
            .pop()
            .expect("expected validator state not to be empty");

        Ok(())
    }

    /// Mark that we've entered a loop.
    fn visit_while_stmt(&mut self, stmt: &mut WhileStmt) -> VisitorResult {
        self.visit_expr(&mut stmt.condition)?;

        self.state.push(State::Loop);

        self.visit_block_stmt(&mut stmt.body)?;

        self.state
            .pop()
            .expect("expected validator state not to be empty");

        Ok(())
    }

    /// Verify that the `continue` statement is inside of a loop.
    fn visit_continue_stmt(&mut self, stmt: &mut ContinueStmt) -> VisitorResult {
        if self.state.last() != Some(&State::Loop) {
            let err_kind = SyntaxErrorKind::ContinueOutsideLoop {
                item: crate::error::Item {
                    span: stmt.position().clone(),
                    content: "".to_string(),
                },
            };
            let err = SyntaxError::new(err_kind);
            Err(err)
        } else {
            Ok(())
        }
    }

    /// Verify that the `break` statement is inside of a loop.
    fn visit_break_stmt(&mut self, stmt: &mut BreakStmt) -> VisitorResult {
        if self.state.last() != Some(&State::Loop) {
            let err_kind = SyntaxErrorKind::BreakOutsideLoop {
                item: crate::error::Item {
                    span: stmt.position().clone(),
                    content: "".to_string(),
                },
            };
            let err = SyntaxError::new(err_kind);
            Err(err)
        } else {
            Ok(())
        }
    }

    /// Verify that the `return` statement is inside of a function body.
    fn visit_return_stmt(&mut self, _return_stmt: &mut ReturnStmt) -> VisitorResult {
        let mut in_function = false;
        for state in self.state.iter().rev() {
            if state == &State::Function {
                in_function = true;
                break;
            }
        }

        if !in_function {
            let kind = SyntaxErrorKind::ReturnOutsideFunction {
                item: Item {
                    span: Span::empty(),
                    content: "".to_string(),
                },
            };

            let err = SyntaxError::new(kind);
            return Err(err);
        }

        Ok(())
    }
}
