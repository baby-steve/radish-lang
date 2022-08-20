use serde::Serialize;

use super::{Span, BlockStmt, Expr, Position, Stmt};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct IfStmt {
    pub condition: Expr,
    pub consequent: BlockStmt,
    #[serde(skip_serializing)]
    pub alternate: Option<Box<Stmt>>,
    pub span: Span,
}

impl Position for IfStmt {
    fn position(&self) -> &Span {
        &self.span
    }
}
