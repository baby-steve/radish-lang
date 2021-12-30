use std::fmt;
use std::borrow::Cow;

use crate::span::Span;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Plus,
    Slash,
    Star,
    Minus,
    Newline,
    LeftParen,
    RightParen,

    True,
    False,

    Number(f64),
    Ident(Box<str>),

    Error(Box<str>),
    Eof,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TokenType::*;

        match *self {
            Plus => write!(f, "Plus"),
            Slash => write!(f, "Slash"),
            Star => write!(f, "Star"),
            Minus => write!(f, "Minus"),
            Newline => write!(f, "Newline"),
            LeftParen => write!(f, "LeftParen"),
            RightParen => write!(f, "RightParen"),
            True => write!(f, "True"),
            False => write!(f, "False"),
            Number(_) => write!(f, "Number"),
            Ident(_) => write!(f, "Ident"),
            Error(_) => write!(f, "Error"),
            Eof => write!(f, "Eof"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub span: Span,
}

impl Token {
    pub fn new(token_type: TokenType, span: Span) -> Token {
        Token { token_type, span }
    }

    fn literal_syntax(&self) -> &'static str {
        use TokenType::*;

        match self.token_type {
            Plus => "+",
            Minus => "-",
            Star => "*",
            Slash => "/",
            Newline => "\n",
            LeftParen => "(",
            RightParen => ")",

            True => "true",
            False => "false",

            Eof => "<Eof>",

            _ => "ERROR"
        }
    }

    pub fn syntax(&self) -> Cow<'static, str> {
        use TokenType::*;

        match &self.token_type {
            Number(val) => val.to_string().into(),
            Ident(id) => id.to_string().into(),
            Error(err) => err.to_string().into(),

            _ => self.literal_syntax().into(),

        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Token {{type: {}, span: {}}}",
            self.token_type, self.span
        )
    }
}
