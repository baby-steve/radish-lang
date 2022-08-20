use serde::Serialize;

use crate::common::Span;

use super::{Expr, Position};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct MapExpr {
    pub properties: Vec<Property>,
    pub span: Span,
}

impl Position for MapExpr {
    fn position(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Property {
    pub key: Expr,
    pub value: Expr,
    pub span: Span,
}

impl Position for Property {
    fn position(&self) -> &Span {
        &self.span
    }
}
