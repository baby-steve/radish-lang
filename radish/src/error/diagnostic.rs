use crate::common::Span;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd)]
pub enum Severity {
    Help,
    Note,
    Warning,
    Error,
    Bug,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd)]
pub enum LabelStyle {
    Primary,
    Secondary,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd)]
pub struct Label {
    pub style: LabelStyle,
    pub span: Span,
    pub message: String,
}

impl Label {
    pub fn new(style: LabelStyle, span: Span) -> Label {
        Label {
            style,
            span,
            message: String::new(),
        }
    }

    pub fn primary(span: Span) -> Label {
        Label::new(LabelStyle::Primary, span)
    }

    pub fn secondary(span: Span) -> Label {
        Label::new(LabelStyle::Secondary, span)
    }

    pub fn with_message(mut self, message: impl ToString) -> Label {
        self.message = message.to_string();
        self
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub labels: Vec<Label>,
    pub notes: Vec<String>,
}

impl Diagnostic {
    pub fn new(severity: Severity) -> Diagnostic {
        Diagnostic {
            severity,
            message: String::new(),
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    pub fn bug() -> Diagnostic {
        Diagnostic::new(Severity::Bug)
    }

    pub fn error() -> Diagnostic {
        Diagnostic::new(Severity::Error)
    }

    pub fn warning() -> Diagnostic {
        Diagnostic::new(Severity::Warning)
    }

    pub fn note() -> Diagnostic {
        Diagnostic::new(Severity::Note)
    }

    pub fn help() -> Diagnostic {
        Diagnostic::new(Severity::Note)
    }

    pub fn with_message(mut self, message: impl ToString) -> Diagnostic {
        self.message = message.to_string();
        self
    }

    pub fn with_labels(mut self, labels: Vec<Label>) -> Diagnostic {
        self.labels = labels;
        self
    }

    pub fn with_notes(mut self, notes: Vec<impl ToString>) -> Diagnostic {
        self.notes = notes.iter().map(|string| string.to_string()).collect();
        self
    }
}

pub trait AsDiagnostic {
    fn diagnostic(&self) -> Diagnostic;
}