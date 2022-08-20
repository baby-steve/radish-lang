use serde::Serialize;

use crate::common::Span;

use super::{Expr, Position};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct LogicalExpr {
    pub lhs: Box<Expr>,
    pub op: LogicalOp,
    pub rhs: Box<Expr>,
    pub span: Span,
}

impl Position for LogicalExpr {
    fn position(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
pub enum LogicalOp {
    And,
    Or,
}
