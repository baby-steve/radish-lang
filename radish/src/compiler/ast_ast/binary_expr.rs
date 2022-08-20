use serde::Serialize;

use crate::common::Span;

use super::{Expr, Position};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub op: BinaryOp,
    pub rhs: Box<Expr>,
    pub span: Span,
}

impl BinaryExpr {
    pub fn new(op: BinaryOp, lhs: Expr, rhs: Expr, span: Span) -> Self {
        Self { lhs: Box::new(lhs), op, rhs: Box::new(rhs), span }
    }
}

impl Position for BinaryExpr {
    fn position(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
    EqualsTo,
    NotEqual,
}
