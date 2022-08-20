use serde::Serialize;

use super::{Span, Expr, Position};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ReturnStmt {
    pub argument: Option<Expr>,
    pub span: Span,
}

impl Position for ReturnStmt {
    fn position(&self) -> &Span {
        &self.span
    }
}
