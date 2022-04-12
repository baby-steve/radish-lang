//! Module containig an AST validator. this pass is meant to catch things that fit
//! into the AST's data structures but are sematically invalid. However it only
//! handles simple checks, not more compilcated things like name resolution.

use crate::common::Span;
use crate::compiler::ast::{Expr, Stmt, VarKind, AST};

use crate::compiler::{visitor::VisitorResult, Visitor};

use crate::compiler::error::{SyntaxError, SyntaxErrorKind};

use crate::error::Item;

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
    fn visit_fun_decl(&mut self, fun: &mut super::FunctionDecl) -> VisitorResult {
        self.state.push(State::Function);

        // TODO: do we need to validate function parameter lists?
        // for param in fun.params.iter_mut() {
        //     self.visit_ident(param)?;
        // }

        self.visit_block_stmt(&mut fun.body)?;

        self.state
            .pop()
            .expect("expected validator state not to be empty");

        Ok(())
    }

    /// Validate the variable's inital value. If it doesn't have an
    /// inital value and its a constant (final) var declaration, then
    /// report an error.
    fn visit_var_decl(
        &mut self,
        id: &mut super::Ident,
        expr: &mut Option<Expr>,
        kind: super::VarKind,
    ) -> VisitorResult {
        match expr {
            Some(expr) => self.visit_expr(expr),
            None if kind == VarKind::Fin => {
                let err_kind = SyntaxErrorKind::MissingConstInit {
                    item: Item::new(&id.pos, &id.name),
                };

                Err(SyntaxError::new(err_kind))
            }
            _ => Ok(()),
        }
    }

    /// Mark that we've entered a loop.
    fn visit_loop_stmt(&mut self, body: &mut Vec<Stmt>) -> VisitorResult {
        self.state.push(State::Loop);

        self.visit_block_stmt(body)?;

        self.state
            .pop()
            .expect("expected validator state not to be empty");

        Ok(())
    }

    /// Mark that we've entered a loop.
    fn visit_while_stmt(&mut self, condition: &mut Expr, body: &mut Vec<Stmt>) -> VisitorResult {
        self.visit_expr(condition)?;

        self.state.push(State::Loop);

        self.visit_block_stmt(body)?;

        self.state
            .pop()
            .expect("expected validator state not to be empty");

        Ok(())
    }

    /// Verify that the `continue` statement is inside of a loop.
    fn visit_continue_stmt(&mut self) -> VisitorResult {
        if self.state.last() != Some(&State::Loop) {
            let err_kind = SyntaxErrorKind::ContinueOutsideLoop {
                item: crate::error::Item {
                    span: Span::empty(), // FIXME: fix this.
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
    fn visit_break_stmt(&mut self) -> VisitorResult {
        if self.state.last() != Some(&State::Loop) {
            let err_kind = SyntaxErrorKind::BreakOutsideLoop {
                item: crate::error::Item {
                    span: Span::empty(), // FIXME: fix this.
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
    fn visit_return_stmt(&mut self, _return_expr: &mut Option<Expr>) -> VisitorResult {
        let mut in_function = false;
        for state in self.state.iter().rev() {
            if state == &State::Function {
                in_function = true;
                break;
            }
        }

        if in_function == false {
            let kind = SyntaxErrorKind::ReturnOutsideFunction {
                item: Item { span: Span::empty(), content: "".to_string() },
            };

            let err = SyntaxError::new(kind);
            return Err(err);
        }

        Ok(())
    }
}


#[cfg(test)]
mod tests {
    use crate::{common::source::Source, compiler::parser::Parser, RadishConfig};

    use super::*;

    #[test]
    fn walk_ast() {
        let src = "1 + 2 - 3 * 4 / 5";
        let source = Source::source(src);
        let config = RadishConfig::new();

        let mut parser = Parser::new(source, &config);

        let mut ast = parser.parse().expect("expected an parser to succeed");

        let res = ast.walk(validate_ast);

        assert!(res.is_ok());
    }
}