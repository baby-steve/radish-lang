use std::borrow::Cow;
use std::fmt;

use crate::span::Span;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    // +
    Plus,
    // /
    Slash,
    // *
    Star,
    // -
    Minus,
    // !
    Bang,
    // =
    Equals,
    // <
    LessThan,
    // <=
    LessThanEquals,
    // >
    GreaterThan,
    // >=
    GreaterThanEquals,
    // ==
    EqualsTo,
    // !=
    NotEqual,
    // \n
    Newline,
    // (
    LeftParen,
    // )
    RightParen,
    // true
    True,
    // false
    False,
    // var (might change)
    Var,

    Number(f64),
    Ident(Box<str>),
    // bool is for if the comment is multiline.
    Comment(Box<str>, bool),

    Error(Box<str>),
    // <Eof>
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
            Bang => write!(f, "Bang"),
            Equals => write!(f, "Equals"),
            LessThan => write!(f, "LessThan"),
            LessThanEquals => write!(f, "LessThanEquals"),
            GreaterThan => write!(f, "GreaterThan"),
            GreaterThanEquals => write!(f, "GreaterThanEquals"),
            EqualsTo => write!(f, "EqualsTo"),
            NotEqual => write!(f, "NotEqual"),
            Newline => write!(f, "Newline"),
            LeftParen => write!(f, "LeftParen"),
            RightParen => write!(f, "RightParen"),
            True => write!(f, "True"),
            False => write!(f, "False"),
            Var => write!(f, "Var"),
            Number(_) => write!(f, "Number"),
            Ident(_) => write!(f, "Ident"),
            Comment(_, _) => write!(f, "Comment"),
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
            Bang => "!",
            Equals => "=",
            LessThan => "<",
            GreaterThan => ">",
            LessThanEquals => "<=",
            GreaterThanEquals => ">=",
            EqualsTo => "==",
            NotEqual => "!=",
            Newline => "\\n",
            LeftParen => "(",
            RightParen => ")",

            True => "true",
            False => "false",
            Var => "var",

            Eof => "<Eof>",

            _ => "ERROR",
        }
    }

    pub fn syntax(&self) -> Cow<'static, str> {
        use TokenType::*;

        match &self.token_type {
            Number(val) => val.to_string().into(),
            Ident(id) => id.to_string().into(),
            Comment(msg, _) => msg.to_string().into(),
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
