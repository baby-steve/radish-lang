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
    // {
    LeftBrace,
    // }
    RightBrace,
    // true
    True,
    // false
    False,
    // nil
    Nil,
    // var (might change)
    Var,
    // print (only temporary)
    Print,

    // number
    Number(f64),
    // id
    Ident(Box<str>),
    // bool is for if the comment is multiline.
    Comment(Box<str>, bool),
    // string
    String(Box<str>),

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
            LeftBrace => write!(f, "LeftBrace"),
            RightBrace => write!(f, "RightBrace"),
            True => write!(f, "True"),
            False => write!(f, "False"),
            Nil => write!(f, "Nil"),
            Var => write!(f, "Var"),
            Print => write!(f, "Print"),
            Number(_) => write!(f, "Number"),
            Ident(_) => write!(f, "Ident"),
            Comment(_, _) => write!(f, "Comment"),
            String(_) => write!(f, "String"),
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
            LessThanEquals => "<=",
            GreaterThan => ">",
            GreaterThanEquals => ">=",
            EqualsTo => "==",
            NotEqual => "!=",
            Newline => "\\n",
            LeftParen => "(",
            RightParen => ")",
            LeftBrace => "{",
            RightBrace => "}",
            True => "true",
            False => "false",
            Nil => "nil",
            Var => "var",
            Print => "print",

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
            String(val) => val.to_string().into(),
            Error(err) => err.to_string().into(),

            _ => self.literal_syntax().into(),
        }
    }

    pub fn lookup_from_string(&self, syntax: &str) -> Option<TokenType> {
        Some(match syntax {
            "+" => TokenType::Plus,
            "-" => TokenType::Minus,
            "*" => TokenType::Star,
            "/" => TokenType::Slash,
            "!" => TokenType::Bang,
            "=" => TokenType::Equals,
            "<" => TokenType::LessThan,
            "<=" => TokenType::LessThanEquals,
            ">" => TokenType::GreaterThan,
            ">=" => TokenType::GreaterThanEquals, 
            "==" => TokenType::EqualsTo,
            "!=" => TokenType::NotEqual,
            "\\n" => TokenType::Newline,
            "(" => TokenType::LeftParen,
            ")" => TokenType::RightParen,
            "{" => TokenType::LeftBrace,
            "}" => TokenType::RightBrace,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "nil" => TokenType::Nil,
            "var" => TokenType::Var,
            "print" => TokenType::Print,
            "<Eof>" => TokenType::Eof,

            _ => return None,
        })
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
