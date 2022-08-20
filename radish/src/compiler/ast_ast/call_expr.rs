use serde::Serialize;

use crate::common::Span;

use super::{Expr, Position};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
    pub span: Span,
}

impl Position for CallExpr {
    fn position(&self) -> &Span {
        &self.span
    }
}
