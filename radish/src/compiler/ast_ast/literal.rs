use serde::Serialize;

use crate::common::Span;

use super::Position;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Literal {
    pub kind: LiteralKind,
    pub span: Span,
}

impl Position for Literal {
    fn position(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum LiteralKind {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil,
}

impl Literal {
    pub fn string(str: String, span: Span) -> Self {
        Self {
            kind: LiteralKind::String(str),
            span,
        }
    }

    pub fn number(num: f64, span: Span) -> Self {
        Self {
            kind: LiteralKind::Number(num),
            span,
        }
    }

    pub fn boolean(val: bool, span: Span) -> Self {
        Self {
            kind: LiteralKind::Boolean(val),
            span,
        }
    }

    pub fn nil(span: Span) -> Self {
        Self {
            kind: LiteralKind::Nil,
            span,
        }
    }
}
