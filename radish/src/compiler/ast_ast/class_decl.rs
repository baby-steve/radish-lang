use serde::Serialize;

use super::{identifier::Identifier, FunctionDecl, Position, Span, Stmt};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ClassDecl {
    pub id: Identifier,
    #[serde(skip_serializing)]
    pub body: Vec<Stmt>,
    pub span: Span,
}

impl Position for ClassDecl {
    fn position(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct MethodDecl {
    pub kind: MethodKind,
    pub body: FunctionDecl,
    pub span: Span,
}

impl MethodDecl {
    pub fn method(body: FunctionDecl) -> Self {
        Self::new(body, MethodKind::Method)
    }

    pub fn constructor(body: FunctionDecl) -> Self {
        Self::new(body, MethodKind::Constructor)
    }

    fn new(body: FunctionDecl, kind: MethodKind) -> Self {
        let span = body.position().clone();
        Self { kind, body, span }
    }
}

impl Position for MethodDecl {
    fn position(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
pub enum MethodKind {
    Constructor,
    Method,
}
