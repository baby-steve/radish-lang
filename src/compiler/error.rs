use crate::common::span::Span;
use crate::error::{AsDiagnostic, Diagnostic, Item, Label};

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub kind: ParseErrorKind,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind) -> ParseError {
        ParseError { kind }
    }
}

impl AsDiagnostic for ParseError {
    fn diagnostic(&self) -> Diagnostic {
        self.report()
    }
}

/// Errors that the parser may encounter.
#[derive(Debug, Clone, PartialEq)]
pub enum ParseErrorKind {
    /// Custom error message.
    Custom { message: String },
    /// Found something unexpected.
    Unexpected { found: Item },
    /// Unexpected end of file
    UnexpectedEof { location: Span },
    /// Expected one thing found another.
    Expected { expected: Item, actual: Item },
    /// Expected an identifier but found something else.
    ExpectedIdent { actual: Item },
    /// Expected end of file, found something else.
    ExpectedEof { actual: Item }, // should this use a Token?
    /// Expected an expression but found something else.
    ExpectedExpression { actual: Item },
    /// Found a mismatched closing delimiter.
    MismatchedDelimiter { first: Item, second: Item },
}

impl ParseError {
    fn report(&self) -> Diagnostic {
        use ParseErrorKind::*;

        match &self.kind {
            Custom { message } => Diagnostic::error().with_message(message),
            Unexpected { found } => Diagnostic::error()
                .with_message(format!("found unexpected `{}`", found.content))
                .with_labels(vec![Label::primary(found.span.clone())]),
            UnexpectedEof { location } => Diagnostic::error()
                .with_message("unexpected end of file")
                .with_labels(vec![Label::primary(location.clone())]),
            Expected { expected, actual } => Diagnostic::error()
                .with_message(format!(
                    "expected `{}`, but got `{}`",
                    expected.content, actual.content
                ))
                .with_labels(vec![Label::primary(actual.span.clone())
                    .with_message(format!("expected `{}`", expected.content))]),
            ExpectedIdent { actual } => Diagnostic::error()
                .with_message(format!(
                    "expected an identifer, but got `{}`",
                    actual.content
                ))
                .with_labels(vec![
                    Label::primary(actual.span.clone()).with_message("expected an identifer")
                ]),
            ExpectedEof { actual } => Diagnostic::error()
                .with_message(format!(
                    "expected end of file, but got `{}`",
                    actual.content
                ))
                .with_labels(vec![
                    Label::primary(actual.span.clone()).with_message("expected end of file")
                ]),
            ExpectedExpression { actual } => Diagnostic::error()
                .with_message(format!(
                    "expected an expression, but got `{}`",
                    actual.content
                ))
                .with_labels(vec![
                    Label::primary(actual.span.clone()).with_message("expected an expression")
                ]),
            MismatchedDelimiter { first, second } => Diagnostic::error()
                .with_message(format!(
                    "mismatched closing delimiter: `{}`",
                    second.content
                ))
                .with_labels(vec![
                    Label::primary(second.span.clone())
                        .with_message("mismatched closing delimiter"),
                    Label::secondary(first.span.clone()).with_message("unclosed delimiter"),
                ]),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SemanticError {
    kind: SemanticErrorKind,
}

impl SemanticError {
    pub fn new(kind: SemanticErrorKind) -> SemanticError {
        SemanticError { kind }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SemanticErrorKind {
    /// break statement outside of a loop.
    BreakOutsideLoop {
        item: Item,
    },
    /// continue statement outside of a loop.
    ContinueOutsideLoop {
        item: Item,
    },
    /// Can't find identifier in current scope.
    UnresolvedIdent {
        item: Item,
    },
    DuplicateIdent {
        first: Item,
        second: Item,
    },
    DuplicateParam {
        param: Item,
    },
}

impl SemanticError {
    fn report(&self) -> Diagnostic {
        use SemanticErrorKind::*;

        match &self.kind {
            BreakOutsideLoop { item } => Diagnostic::error()
                .with_message("`break` outside of a loop")
                .with_labels(vec![
                    Label::primary(item.span.clone())
                        .with_message("cannot `break` outside of a loop"),
                ]),
            ContinueOutsideLoop { item } => Diagnostic::error()
                .with_message("`continue` outside of a loop")
                .with_labels(vec![
                    Label::primary(item.span.clone())
                        .with_message("cannot `continue` outside of a loop")
                ]),
            UnresolvedIdent { item } => Diagnostic::error()
                .with_message(format!("cannot find identifier `{}` in this scope", item.content))
                .with_labels(vec![
                    Label::primary(item.span.clone())
                        .with_message("not found in this scope"),
                ]),
            DuplicateIdent { first, second } => Diagnostic::error()
                .with_message(format!("the name `{}` is defined multiple times", first.content))
                .with_labels(vec![
                    Label::secondary(first.span.clone())
                        .with_message(&format!("previous definition of `{}` here", first.content)),
                    Label::primary(second.span.clone())
                        .with_message(&format!("`{}` redefined here", second.content))
                ])
                .with_notes(vec!["identifiers can only be defined once in a scope"]),
            DuplicateParam { param } => Diagnostic::error()
                .with_message(format!("identifer `{}` is bound more than once", param.content))
                .with_labels(vec![
                    Label::primary(param.span.clone())
                        .with_message("used as parameter more than once")
                ]),
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct CompileError {
    msg: String,
}

impl CompileError {
    pub fn new(msg: String) -> CompileError {
        CompileError { msg }
    }
}