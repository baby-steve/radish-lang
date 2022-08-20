use serde::Serialize;

use crate::common::Span;

use super::{Expr, Position};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ArrayExpr {
    pub elements: Vec<Expr>,
    pub span: Span,
}

impl Position for ArrayExpr {
    fn position(&self) -> &Span {
        &self.span
    }
}
