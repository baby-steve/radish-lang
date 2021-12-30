use std::fmt;
use std::borrow::Cow;

use crate::span::Span;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Plus,
    Slash,
    Star,
    Minus,

    LeftParen,
    RightParen,

    Number(f64),
    True,
    False,
    Ident(Box<str>),

    Error(Box<str>),
    Eof,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TokenType::Plus => write!(f, "Plus"),
            TokenType::Slash => write!(f, "Slash"),
            TokenType::Star => write!(f, "Star"),
            TokenType::Minus => write!(f, "Minus"),
            TokenType::LeftParen => write!(f, "LeftParen"),
            TokenType::RightParen => write!(f, "RightParen"),
            TokenType::Number(_) => write!(f, "Number"),
            TokenType::True => write!(f, "True"),
            TokenType::False => write!(f, "False"),
            TokenType::Ident(_) => write!(f, "Ident"),
            TokenType::Error(_) => write!(f, "Error"),
            TokenType::Eof => write!(f, "Eof"),
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
