use std::{
    borrow::Cow,
    fmt,
};

use crate::common::span::Span;

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
    // %
    Percent,
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
    // +=
    PlusEquals,
    // -=
    MinusEquals,
    // *=
    MultiplyEquals,
    // /=
    DivideEquals,
    // %=
    ModuloEquals,
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
    // ,
    Comma,
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
    // and
    And,
    // or
    Or,
    // if
    If,
    // then
    Then,
    // else
    Else,
    // endif
    EndIf,
    // loop
    Loop,
    // while
    While,
    // endloop
    EndLoop,
    // break
    Break,
    // continue
    Continue,
    // fun
    Fun,
    // return
    Return,

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
    // <empty>
    Empty,
}

impl TokenType {
    fn literal_syntax(&self) -> &'static str {
        use TokenType::*;

        match self {
            Plus => "+",
            Minus => "-",
            Star => "*",
            Slash => "/",
            Percent => "%",
            Bang => "!",
            Equals => "=",
            LessThan => "<",
            LessThanEquals => "<=",
            GreaterThan => ">",
            GreaterThanEquals => ">=",
            EqualsTo => "==",
            NotEqual => "!=",
            PlusEquals => "+=",
            MinusEquals => "-=",
            MultiplyEquals => "*=",
            DivideEquals => "/=",
            ModuloEquals => "%=",
            Newline => "\\n",
            LeftParen => "(",
            RightParen => ")",
            LeftBrace => "{",
            RightBrace => "}",
            Comma => ",",
            True => "true",
            False => "false",
            Nil => "nil",
            Var => "var",
            Print => "print",
            And => "and",
            Or => "or",
            If => "if",
            Then => "then",
            Else => "else",
            EndIf => "endif",
            Loop => "loop",
            While => "while",
            EndLoop => "endloop",
            Break => "break",
            Continue => "continue",
            Fun => "fun",
            Return => "return",

            Eof => "<Eof>",

            _ => "ERROR",
        }
    }

    pub fn syntax(&self) -> Cow<'static, str> {
        use TokenType::*;

        match &self {
            Number(val) => val.to_string().into(),
            Ident(id) => id.to_string().into(),
            Comment(msg, _) => msg.to_string().into(),
            String(val) => val.to_string().into(),
            Error(err) => err.to_string().into(),

            _ => self.literal_syntax().into(),
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

    pub fn empty() -> Token {
        Token::new(TokenType::Empty, Span::empty())
    }

    pub fn syntax(&self) -> Cow<'static, str> {
        self.token_type.syntax()
    }

    pub fn is_delimiter(&self) -> bool {
        match &self.token_type {
            TokenType::RightBrace
            | TokenType::Else
            | TokenType::EndLoop
            | TokenType::EndIf
            | TokenType::RightParen => true,
            _ => false,
        }
    }

    pub fn lookup_from_string(&self, syntax: &str) -> Option<TokenType> {
        Some(match syntax {
            "+" => TokenType::Plus,
            "-" => TokenType::Minus,
            "*" => TokenType::Star,
            "/" => TokenType::Slash,
            "%" => TokenType::Percent,
            "!" => TokenType::Bang,
            "=" => TokenType::Equals,
            "<" => TokenType::LessThan,
            "<=" => TokenType::LessThanEquals,
            ">" => TokenType::GreaterThan,
            ">=" => TokenType::GreaterThanEquals, 
            "==" => TokenType::EqualsTo,
            "!=" => TokenType::NotEqual,
            "+=" => TokenType::PlusEquals,
            "-=" => TokenType::MinusEquals,
            "*=" => TokenType::MultiplyEquals,
            "/=" => TokenType::DivideEquals,
            "%=" => TokenType::ModuloEquals,
            "\\n" => TokenType::Newline,
            "(" => TokenType::LeftParen,
            ")" => TokenType::RightParen,
            "{" => TokenType::LeftBrace,
            "}" => TokenType::RightBrace,
            "," => TokenType::Comma,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "nil" => TokenType::Nil,
            "var" => TokenType::Var,
            "print" => TokenType::Print,
            "and" => TokenType::And,
            "or" => TokenType::Or,
            "if" => TokenType::If,
            "then" => TokenType::Then,
            "else" => TokenType::Else,
            "endif" => TokenType::EndIf,
            "loop" => TokenType::Loop,
            "while" => TokenType::While,
            "endloop" => TokenType::EndLoop,
            "break" => TokenType::Break,
            "continue" => TokenType::Continue,
            "fun" => TokenType::Fun,
            "return" => TokenType::Return,
            "<Eof>" => TokenType::Eof,

            _ => return None,
        })
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Token {{type: {:?}, span: {:?}}}",
            self.token_type, self.span
        )
    }
}

/*
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
            And => write!(f, "And"),
            Or => write!(f, "Or"),
            If => write!(f, "If"),
            Then => write!(f, "Then"),
            End => write!(f, "End"),
            Number(_) => write!(f, "Number"),
            Ident(_) => write!(f, "Ident"),
            Comment(_, _) => write!(f, "Comment"),
            String(_) => write!(f, "String"),
            Error(_) => write!(f, "Error"),
            Eof => write!(f, "Eof"),
        }
    }
} */