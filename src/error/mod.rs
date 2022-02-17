pub mod diagnostic;
pub mod renderer;
pub mod views;

use termcolor::WriteColor;

pub use diagnostic::{AsDiagnostic, Diagnostic, Label, LabelStyle};
use crate::common::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    pub span: Span,
    pub content: String,
}

impl Item {
    pub fn new(span: &Span, content: impl Into<String>) -> Item {
        let content = content.into();
        Item {
            span: span.clone(),
            content,
        }
    }
}

/// Emit a diagnostic
pub fn emit(
    writer: &mut dyn WriteColor, 
    diagnostic: &Diagnostic,
    display_style: u8, // HACK make this an enum or something 
) -> Result<(), String> {
    use self::renderer::Renderer;
    use self::views::{ShortDiagnostic, RichDiagnostic};

    let mut renderer = Renderer::new(writer);
    match display_style {
        0 => ShortDiagnostic::new(&diagnostic).render(&mut renderer),
        _ => RichDiagnostic::new(&diagnostic).render(&mut renderer),
    }
}