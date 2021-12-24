use std::fmt;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    Plus,
    Slash,
    Star,
    Minus,

    Number,

    Error,
    Eof,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TokenType::Plus => write!(f, "Plus"),
            TokenType::Slash => write!(f, "Slash"),
            TokenType::Star => write!(f, "Star"),
            TokenType::Minus => write!(f, "Minus"),
            TokenType::Number => write!(f, "Number"),
            TokenType::Error => write!(f, "Error"),
            TokenType::Eof => write!(f, "Eof"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
    pub span: Span,
}

impl Token {
    pub fn new(token_type: TokenType, value: String, span: Span) -> Token {
        Token {
            token_type,
            value,
            span,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Token {{type: {}, value: {}, span: {}}}", self.token_type, self.value, self.span)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub line: usize,
    pub offset: usize,
}

impl Span {
    pub fn new(line: usize, offset: usize) -> Span {
        Span { line, offset, }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "line: {}, offset: {}", self.line, self.offset)
    }
}