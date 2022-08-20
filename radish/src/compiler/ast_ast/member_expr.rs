use serde::Serialize;

use crate::common::Span;

use super::{Expr, Position};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct MemberExpr {
    pub object: Box<Expr>,
    pub property: Box<Expr>,
    pub computed: bool,
    pub span: Span,
}

impl Position for MemberExpr {
    fn position(&self) -> &Span {
        &self.span
    }
}
