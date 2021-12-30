use crate::span::Span;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
/// An error for when some unexpected token was found.
pub enum UnexpectedError {
    UnexpectedToken(String),
    UnexpectedEOF,
}

impl fmt::Display for UnexpectedError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use UnexpectedError::*;

        match &self {
            UnexpectedToken(token) => f.write_str(&format!("found unexpected token '{}'.", token)),
            UnexpectedEOF => f.write_str(&format!("found unexpected '<Eof>'.")),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
/// An error for when one thing is expected, but found something else.
pub enum ExpectedError {
    ExpectedExpression(String),
    ExpectedRightParen(String),
}

impl fmt::Display for ExpectedError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ExpectedError::*;

        match &self {
            ExpectedExpression(p) => f.write_str(&format!("Expected expression, {}", p)),
            ExpectedRightParen(p) => f.write_str(&format!("Expected right parenthesis, {}", p)),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum SyntaxError {
    ExpectedError(ExpectedError),
    UnexpectedError(UnexpectedError),
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            SyntaxError::UnexpectedError(e) => e.fmt(f),
            SyntaxError::ExpectedError(e) => e.fmt(f),
        }
    }
}

impl SyntaxError {
    pub fn is_unexpected(&self) -> bool {
        matches!(self, Self::UnexpectedError(_))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ParserError {
    pub error: SyntaxError,
    pub span: Span,
}

impl ParserError {
    pub fn new(error: SyntaxError, span: &Span) -> ParserError {
        ParserError {
            error,
            span: Span::from(span),
        }
    }

    pub fn is_unexpected(&self) -> bool {
        self.error.is_unexpected()
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let message = format!("{}", self.error);

        write!(f, "Error: {}\n{}", &message, self.span)
    }
}