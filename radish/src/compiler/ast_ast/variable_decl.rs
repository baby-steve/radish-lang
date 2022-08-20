use serde::Serialize;

use super::{Span, identifier::Identifier, Expr, Position};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct VariableDecl {
    pub id: Identifier,
    pub init: Option<Expr>,
    pub kind: VariableKind,
    pub span: Span,
}

impl Position for VariableDecl {
    fn position(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
pub enum VariableKind {
    Variable,
    Final,
}
