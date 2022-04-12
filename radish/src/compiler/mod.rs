//! Module containing the Radish language's frontend.

pub mod analysis;
pub mod ast;
pub mod compiler;
pub mod error;
pub mod parser;
pub mod scanner;
pub mod scope;
pub mod token;
pub mod visitor;
pub mod validator;

pub use ast::*;
pub use compiler::Compiler;
pub use error::SyntaxError;
pub use scope::ScopeMap;
pub use visitor::Visitor;

pub use std::cell::RefCell;
pub use std::rc::Rc;

use validator::validate_ast;
use analysis::resolve_symbols;

// TODO: rename this.
// TODO: move this to a better place.
// TODO: rewrite this.
pub fn check(ast: &mut AST) -> Result<(), SyntaxError> {
    ast.walk(validate_ast)?;
    ast.walk(resolve_symbols)
}