use std::fmt;

use crate::span::Span;

#[derive(Debug, PartialEq)]
pub enum InternalError {
    ParseFloatError,
}

impl fmt::Display for InternalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use InternalError::*;

        match &self {
            ParseFloatError => f.write_str("Parse float error while parsing "),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum SyntaxError {
    UnexpectedToken,
    UnexpectedEOF,
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use SyntaxError::*;

        match &self {
            UnexpectedToken => f.write_str("Unexpected token "),
            UnexpectedEOF => f.write_str("Unexpected end of file")
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ErrorType {
    InternalError(InternalError),
    SyntaxError(SyntaxError),
}

impl From<InternalError> for ErrorType {
    fn from(e: InternalError) -> ErrorType {
        ErrorType::InternalError(e)
    }
}

impl From<SyntaxError> for ErrorType {
    fn from(e: SyntaxError) -> ErrorType {
        ErrorType::SyntaxError(e)
    }
}

impl fmt::Display for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorType::InternalError(e) => e.fmt(f),
            ErrorType::SyntaxError(e) => e.fmt(f),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ParserError {
    pub error: ErrorType,
    pub span: Span,
}

impl ParserError {
    pub fn new(error: ErrorType, span: Span) -> ParserError {
        ParserError { error, span }
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let message = format!("{}", self.error);

        write!(f, "Error: {}\n{}", &message, self.span)
    }
}