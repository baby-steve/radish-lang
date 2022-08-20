use serde::Serialize;

use crate::common::Span;

use super::Position;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ThisExpr {
    pub span: Span,
}

impl Position for ThisExpr {
    fn position(&self) -> &Span {
        &self.span
    }
}
