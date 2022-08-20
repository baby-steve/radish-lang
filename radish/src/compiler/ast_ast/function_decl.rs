use serde::Serialize;

use crate::compiler::hoist::Scope;

use super::{identifier::Identifier, BlockStmt, Position, Span};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct FunctionDecl {
    pub id: Identifier,
    pub params: Vec<Identifier>,
    pub body: BlockStmt,
    #[serde(skip_serializing)]
    pub scope: Option<Scope>,
    pub span: Span,
}

impl Position for FunctionDecl {
    fn position(&self) -> &Span {
        &self.span
    }
}
