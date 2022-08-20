use serde::Serialize;

use super::{block_stmt::BlockStmt, Span, Expr, Position};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: BlockStmt,
    pub span: Span,
}

impl Position for WhileStmt {
    fn position(&self) -> &Span {
        &self.span
    }
}
