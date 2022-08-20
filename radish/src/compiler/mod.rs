//! Module containing the Radish language's frontend.

mod analysis;
// pub mod analysis_analysis;
pub mod ast;
pub mod ast_ast;
pub mod codegen;
pub mod error;
mod hoist;
// mod hoist_hoist;
pub mod parser;
// pub mod parser_parser;
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
