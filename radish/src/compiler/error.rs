use crate::common::span::Span;
use crate::error::{AsDiagnostic, Diagnostic, Item, Label};

#[derive(Debug, Clone, PartialEq)]
pub struct SyntaxError {
    pub kind: SyntaxErrorKind,
    cause: Option<Box<SyntaxError>>,
}

impl SyntaxError {
    pub fn new(kind: SyntaxErrorKind) -> SyntaxError {
        SyntaxError { kind, cause: None }
    }

    pub fn set_cause(mut self, cause: SyntaxError) -> SyntaxError {
        self.cause = Some(Box::new(cause));
        self
    }

    pub fn get_cause(self) -> Option<SyntaxError> {
        if let Some(err) = self.cause {
            Some(*err)
        } else {
            None
        }
    }

    pub fn is_unexpected_eof(self) -> bool {
        if let SyntaxErrorKind::UnexpectedEof { location: _ } = self.kind {
            return true;
        }

        if self.cause != None {
            return self.cause.unwrap().is_unexpected_eof();
        }

        false
    }
}

impl AsDiagnostic for SyntaxError {
    fn diagnostic(&self) -> Diagnostic {
        self.report()
    }
}

/// Errors that the parser may encounter.
#[derive(Debug, Clone, PartialEq)]
pub enum SyntaxErrorKind {
    /// Custom error message.
    Custom {
        message: String,
    },
    /// Found something unexpected.
    Unexpected {
        found: Item,
    },
    /// Unexpected end of file
    UnexpectedEof {
        location: Span,
    },
    /// Expected one thing found another.
    Expected {
        expected: Item,
        actual: Item,
    },
    /// Expected an identifier but found something else.
    ExpectedIdent {
        actual: Item,
    },
    /// Expected end of file, found something else.
    ExpectedEof {
        actual: Item,
    },
    /// Expected an expression but found something else.
    ExpectedExpression {
        actual: Item,
    },
    /// Expected a newline
    ExpectedNewline {
        actual: Item,
    },
    /// Found a mismatched closing delimiter.
    MismatchedDelimiter {
        first: Item,
        second: Item,
    },
    /// break statement outside of a loop.
    BreakOutsideLoop {
        item: Item,
    },
    /// continue statement outside of a loop.
    ContinueOutsideLoop {
        item: Item,
    },
    /// return statement outside of a function.
    ReturnOutsideFunction {
        item: Item,
    },
    /// Can't find identifier in current scope.
    UnresolvedIdent {
        item: Item,
    },
    /// Found more then one identifier in the current scope.
    DuplicateIdent {
        first: Item,
        second: Item,
    },
    /// Used a name for a parameter more than once.
    DuplicateParam {
        param: Item,
    },
    MissingConstInit {
        item: Item,
    },
    AssignToConst {
        item: Item,
    },
}

impl SyntaxError {
    pub fn report(&self) -> Diagnostic {
        use SyntaxErrorKind::*;

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
            ExpectedNewline { actual } => Diagnostic::error()
                .with_message("expected a newline")
                .with_labels(vec![
                    Label::primary(actual.span.clone()).with_message("expected a newline here")
                ])
                .with_notes(vec![
                    "statements are newline terminated",
                    "try inserting a newline",
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
            BreakOutsideLoop { item } => Diagnostic::error()
                .with_message("`break` outside of a loop")
                .with_labels(vec![Label::primary(item.span.clone())
                    .with_message("cannot `break` outside of a loop")]),
            ContinueOutsideLoop { item } => Diagnostic::error()
                .with_message("`continue` outside of a loop")
                .with_labels(vec![Label::primary(item.span.clone())
                    .with_message("cannot `continue` outside of a loop")]),
            ReturnOutsideFunction { item } => Diagnostic::error()
                .with_message("`return` outside of function body")
                .with_labels(vec![Label::primary(item.span.clone())])
                    .with_message("cannot `return` outside of a function"),
            UnresolvedIdent { item } => Diagnostic::error()
                .with_message(format!(
                    "cannot find identifier `{}` in this scope",
                    item.content
                ))
                .with_labels(vec![
                    Label::primary(item.span.clone()).with_message("not found in this scope")
                ]),
            DuplicateIdent { first, second } => Diagnostic::error()
                .with_message(format!(
                    "the name `{}` is defined multiple times",
                    first.content
                ))
                .with_labels(vec![
                    Label::secondary(first.span.clone())
                        .with_message(&format!("previous definition of `{}` here", first.content)),
                    Label::primary(second.span.clone())
                        .with_message(&format!("`{}` redefined here", second.content)),
                ])
                .with_notes(vec!["identifiers can only be defined once in a scope"]),
            DuplicateParam { param } => Diagnostic::error()
                .with_message(format!(
                    "identifer `{}` is bound more than once",
                    param.content
                ))
                .with_labels(vec![Label::primary(param.span.clone())
                    .with_message("used as parameter more than once")]),
            MissingConstInit { item } => Diagnostic::error()
                .with_message("missing initalizer in constant declaration")
                .with_labels(vec![
                    Label::primary(item.span.clone()).with_message("missing initalizer")
                ])
                .with_notes(vec!["add a definition for the constant: `= <expr>`"]),
            AssignToConst { item } => Diagnostic::error()
                .with_message(&format!(
                    "attempt to assign to constant variable: `{}`",
                    &item.content
                ))
                .with_labels(vec![
                    Label::primary(item.span.clone()).with_message("cannot assign")
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn error_cause() {
        let unexpected_eof = SyntaxError::new(SyntaxErrorKind::UnexpectedEof {
            location: Span::empty(),
        });

        assert!(unexpected_eof.clone().is_unexpected_eof());

        let expected_expr = SyntaxError::new(SyntaxErrorKind::ExpectedExpression {
            actual: Item::new(&Span::empty(), "<eof>"),
        })
        .set_cause(unexpected_eof);

        assert!(expected_expr.is_unexpected_eof());
    }
}
