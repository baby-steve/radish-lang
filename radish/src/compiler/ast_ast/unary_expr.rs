use serde::Serialize;

use crate::common::Span;

use super::{Expr, Position};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub argument: Box<Expr>,
    pub span: Span,
}

impl Position for UnaryExpr {
    fn position(&self) -> &crate::common::Span {
        &self.span
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
pub enum UnaryOp {
    // Plus,
    Minus,
    Not,
}
