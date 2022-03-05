pub mod diagnostic;
pub mod renderer;
pub mod views;

use termcolor::WriteColor;

pub use diagnostic::{AsDiagnostic, Diagnostic, Label, LabelStyle};
use crate::common::span::Span;

use std::io;

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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DisplayStyle {
    Verbose,
    Short,
}

/// Emit a diagnostic
pub fn emit(
    writer: &mut dyn WriteColor, 
    diagnostic: &Diagnostic,
    display_style: DisplayStyle,
) -> io::Result<()> {
    use self::renderer::Renderer;
    use self::views::{ShortDiagnostic, RichDiagnostic};

    let mut renderer = Renderer::new(writer);
    match display_style {
        DisplayStyle::Short => ShortDiagnostic::new(&diagnostic).render(&mut renderer),
        DisplayStyle::Verbose => RichDiagnostic::new(&diagnostic).render(&mut renderer),
    }
}