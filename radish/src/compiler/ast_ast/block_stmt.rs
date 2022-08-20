use serde::Serialize;

use super::{Span, Position, Stmt};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct BlockStmt {
    pub body: Vec<Stmt>,
    pub span: Span,
}

impl Position for BlockStmt {
    fn position(&self) -> &Span {
        &self.span
    }
}
