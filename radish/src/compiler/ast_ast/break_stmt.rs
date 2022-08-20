use serde::Serialize;

use super::{Span, Position};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct BreakStmt {
    pub span: Span,
}

impl Position for BreakStmt {
    fn position(&self) -> &Span {
        &self.span
    }
}
