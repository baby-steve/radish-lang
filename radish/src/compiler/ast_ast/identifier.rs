use std::cmp::Ordering;

use serde::Serialize;

use crate::compiler::hoist::VarScope;

use super::{Span, Position};

/// An identifier node like `foo` or `Pineapple`.  
#[derive(Debug, Clone, Serialize)]
pub struct Identifier {
    /// Identifier's name.
    pub name: String,
    /// Location in the source file.
    pub span: Span,
    /// Calculated by the ``hoist`` pass. This can be different things depending on what scope
    /// type this identifier has. If the identifier is global, then this is the index in the 
    /// compiled module's symbol table. If it's a local then this is an operator stack index.
    /// If it's an upvalue then its an index into a closure's upvalue array.  
    pub index: u32,
    /// What scope this identifier has.
    pub typ: VarScope,
}

impl Identifier {
    pub fn new<N: ToString>(name: N, span: Span) -> Self {
        Self {
            name: name.to_string(),
            span,
            index: 0,
            // FIXME: an Identifier's default scope type should be 'Unknown'.
            typ: VarScope::Global,
        }
    }
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl PartialOrd for Identifier {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Identifier {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(&other.name)
    }
}

impl Eq for Identifier {}

impl std::hash::Hash for Identifier {
    fn hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        self.name.hash(state);
        state.finish();
    }
}

impl Position for Identifier {
    fn position(&self) -> &Span {
        &self.span
    }
}
