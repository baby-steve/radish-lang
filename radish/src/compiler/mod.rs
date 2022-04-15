//! Module containing the Radish language's frontend.

mod analysis;
pub mod ast;
pub mod codegen;
pub mod error;
pub mod parser;
pub mod pipeline;
pub mod scanner;
pub mod scope;
pub mod token;
mod validator;
pub mod visitor;

pub use ast::*;
pub use codegen::Compiler;
pub use error::SyntaxError;
pub use parser::Parser;
pub use scope::ScopeMap;
pub use visitor::Visitor;

pub use std::cell::RefCell;
pub use std::rc::Rc;

pub use analysis::resolve_symbols;
pub use validator::validate_ast;

// TODO: rename this.
// TODO: move this to a better place.
// TODO: rewrite this.
pub fn check(ast: &mut AST) -> Result<(), SyntaxError> {
    ast.walk(validate_ast)?;
    ast.walk(resolve_symbols)
}
