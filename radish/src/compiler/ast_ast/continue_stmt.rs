use serde::Serialize;

use super::{Span, Position};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ContinueStmt {
    pub span: Span,
}

impl Position for ContinueStmt {
    fn position(&self) -> &Span {
        &self.span
    }
}
