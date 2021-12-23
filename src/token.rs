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

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
}

impl Token {
    pub fn new(token_type: TokenType, value: String) -> Token {
        Token {
            token_type,
            value: value,
        }
    }
}