use serde::Serialize;

use super::{block_stmt::BlockStmt, Span, Position};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct LoopStmt {
    pub body: BlockStmt,
    pub span: Span,
}

impl Position for LoopStmt {
    fn position(&self) -> &Span {
        &self.span
    }
}
