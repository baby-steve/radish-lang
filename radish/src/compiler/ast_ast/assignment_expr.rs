use serde::Serialize;
use crate::common::Span;
use super::{Expr, Position};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct AssignmentExpr {
    pub lhs: Box<Expr>,
    pub op: OpAssignment,
    pub rhs: Box<Expr>,
    pub span: Span,
}

impl Position for AssignmentExpr {
    fn position(&self) -> &Span {
        &self.span
    }
}

/// An assignment operand.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum OpAssignment {
    Equals,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
}
