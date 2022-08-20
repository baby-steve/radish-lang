use std::{
    borrow::Cow,
    fmt,
};

use crate::common::Span;

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
    // [
    LeftBracket,
    // ]
    RightBracket,
    // ,
    Comma,
    // .
    Dot,
    // :
    Colon,
    // true
    True,
    // false
    False,
    // nil
    Nil,
    // var
    Var,
    // fin
    Fin,
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
    // class
    Class,
    // con
    Con,
    // this
    This,
    // return
    Return,
    // for
    For,
    // import
    Import,
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
            LeftBracket => "[",
            RightBracket => "]",
            LeftBrace => "{",
            RightBrace => "}",
            Comma => ",",
            Dot => ".",
            Colon => ":",
            True => "true",
            False => "false",
            Nil => "nil",
            Var => "var",
            Fin => "fin",
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
            Class => "class",
            Con => "con",
            This => "this",
            Return => "return",
            For => "for",
            Import => "import",

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
        matches!(&self.token_type, TokenType::RightBrace
            | TokenType::Else
            | TokenType::EndLoop
            | TokenType::EndIf
            | TokenType::RightParen)
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
