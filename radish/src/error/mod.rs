pub mod diagnostic;
pub mod renderer;
pub mod views;

use termcolor::WriteColor;

pub use diagnostic::{AsDiagnostic, Diagnostic, Label, LabelStyle};
use crate::{common::span::Span, compiler::SyntaxError, vm::trace::Trace};

use std::{io, fmt};

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


#[derive(Debug, Clone)]
pub struct IOError(std::sync::Arc<std::io::Error>);

impl fmt::Display for IOError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Eq for IOError {}

impl PartialEq for IOError {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(&*self.0, &*other.0)
    }
}

impl From<std::io::Error> for IOError {
    fn from(err: std::io::Error) -> IOError {
        IOError(std::sync::Arc::new(err))
    }
}
 
#[derive(Debug, Clone, PartialEq)]
pub enum RadishError {
    CompilerError(SyntaxError),
    RuntimeError(Trace),
    IOError(IOError),
    Other(String),
}

impl From<SyntaxError> for RadishError {
    fn from(err: SyntaxError) -> RadishError {
        RadishError::CompilerError(err)
    }
}

impl From<Trace> for RadishError {
    fn from(err: Trace) -> RadishError {
        RadishError::RuntimeError(err)
    }
}

impl From<std::io::Error> for RadishError {
    fn from(err: std::io::Error) -> RadishError {
        RadishError::IOError(err.into())
    }
}

impl From<String> for RadishError {
    fn from(err: String) -> Self {
        RadishError::Other(err)
    }
}

impl From<&str> for RadishError {
    fn from(err: &str) -> Self {
        RadishError::Other(err.to_string())
    }
}

impl RadishError {
    pub fn emit(&self) {
        match &self {
            RadishError::CompilerError(err) => {
                use termcolor::{ColorChoice, StandardStream};
                let mut temp_stderr = StandardStream::stderr(ColorChoice::Always);
                emit(
                    &mut temp_stderr,
                    &err.report(),
                    DisplayStyle::Verbose,
                )
                .unwrap();
            }
            RadishError::RuntimeError(err) => print!("{}", err),
            RadishError::IOError(err) => print!("{}", err),
            RadishError::Other(err) => print!("{}", err),
        }
    }
}
