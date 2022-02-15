use std::io::{self};
use termcolor::{Color, ColorSpec, WriteColor};

use crate::common::span::Span;

pub mod diagnostic;

pub use diagnostic::{AsDiagnostic, Diagnostic, Label};

pub struct Renderer<'writer> {
    pub writer: &'writer mut dyn WriteColor,
}
/*
impl<'writer> Renderer<'writer> {
    pub fn new(writer: &'writer mut dyn WriteColor) -> Renderer<'writer> {
        Renderer { writer }
    }

    pub fn render(&self, diagnostic: impl AsDiagnostic) {}

    /// Render diagnostic's severity level and message
    /// 
    /// ```text
    /// error: some error message
    /// ```
    fn render_header(&mut self, severity: Severity, message: &str) -> Result<(), String> {
        // write severity level name
        match severity {
            Severity::Bug => write!(self, "bug"),
            Severity::Error => write!(self, "error"),
            Severity::Warning => write!(self, "warning"),
            Severity::Help => write!(self, "help"),
            Severity::Note => write!(self, "note"),
        }

        // write message
        write!(self, ": {}", message);

        writeln!(self);

        Ok(())
    }

    /// Render diagnostic's location
    /// 
    /// ```text
    ///  --> path/to/file.rdsh:12:3
    /// ```
    fn render_location(&mut self, span: Span, padding: usize) -> Result<(), String> {
        let (line_num, col_num) = Span::get_line_index(&span.source.contents, span.start)
            .expect("Could not find line and column numbers");

        write!(
            self,
            "{} --> {name}:{line_num}:{col_num}",
            " ".repeat(padding),
            name = span.source.as_ref().clone().path.to_string_lossy(),
            line_num = line_num,
            col_num = col_num,
        );

        writeln!(self);

        Ok(())
    }
}*/

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
/*
pub struct Emitter<'a> {
    renderer: Renderer<'a>,
}

impl<'a> Emitter<'a> {
    pub fn new(writer: &'a mut dyn WriteColor) -> Emitter<'a> {
        Emitter {
            renderer: Renderer::new(writer)
        }
    }

    pub fn render(&self, diagnostic: Diagnostic) {
        self.renderer.render_header(diagnostic.severity, &diagnostic.message);
    }
}*/