use serde::Serialize;
use crate::common::Span;
use super::{Expr, Position};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ExpressionStmt {
    pub expr: Expr,
    pub span: Span,
}

impl ExpressionStmt {
    pub fn new(expr: Expr) -> Self {
        let span = Span::from(expr.position());

        Self { expr, span }
    }
}

impl Position for ExpressionStmt {
    fn position(&self) -> &Span {
        &self.span
    }
}
