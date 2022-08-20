use serde::Serialize;

use super::{Span, Position, Identifier};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ImportStmt {
    pub source: String,
    pub span: Span,
}

impl ImportStmt {
    pub fn name(&self) -> Option<Identifier> {
        use std::path::Path;

        let path = Path::new(&self.source);

        let file_path = path.file_name();

        file_path.map(|p| p.to_str().unwrap()).map(|name| {
            let span = self.span.clone();
            Identifier::new(name.to_string(), span)
        })
    }
}

impl Position for ImportStmt {
    fn position(&self) -> &Span {
        &self.span
    }
}
