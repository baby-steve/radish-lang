use std::rc::Rc;

use crate::common::{source::Source, span::Span};
use crate::compiler::token::{Token, TokenType};

pub struct Scanner {
    pub source: Rc<Source>,
    pub current: usize,
    pub previous: usize,
}

impl Scanner {
    pub fn new(source: Rc<Source>) -> Scanner {
        Scanner {
            source: Rc::clone(&source),
            current: 0,
            previous: 0,
        }
    }

    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace();
        let c = self.advance();

        match c {
            Some("+") => self.make_token(TokenType::Plus),
            Some("-") => self.make_token(TokenType::Minus),
            Some("/") => {
                if self.match_("/") {
                    self.single_line_comment()
                } else {
                    self.make_token(TokenType::Slash)
                }
            }
            Some("*") => self.make_token(TokenType::Star),
            Some("!") => {
                if self.match_("=") {
                    self.make_token(TokenType::NotEqual)
                } else {
                    self.make_token(TokenType::Bang)
                }
            }
            Some("<") => {
                if self.match_("=") {
                    self.make_token(TokenType::LessThanEquals)
                } else {
                    self.make_token(TokenType::LessThan)
                }
            }
            Some(">") => {
                if self.match_("=") {
                    self.make_token(TokenType::GreaterThanEquals)
                } else {
                    self.make_token(TokenType::GreaterThan)
                }
            }
            Some("=") => {
                if self.match_("=") {
                    self.make_token(TokenType::EqualsTo)
                } else {
                    self.make_token(TokenType::Equals)
                }
            }
            Some("\n") => self.make_token(TokenType::Newline),
            Some("(") => self.make_token(TokenType::LeftParen),
            Some(")") => self.make_token(TokenType::RightParen),
            Some("{") => self.make_token(TokenType::LeftBrace),
            Some("}") => self.make_token(TokenType::RightBrace),
            Some("\"") => self.scan_string(),
            None => self.make_token(TokenType::Eof),
            _ if is_alpha(c.unwrap()) => self.identifier(),
            _ if is_digit(c.unwrap()) => self.number(),
            _ => {
                let msg = format!("{}", c.unwrap());
                return self.make_error_token(&msg);
            }
        }
    }

    fn make_token(&mut self, token_type: TokenType) -> Token {
        let token = Token::new(
            token_type,
            Span::new(Rc::clone(&self.source), self.previous, self.current),
        );
        self.previous = self.current;
        token
    }

    fn make_error_token(&mut self, msg: &str) -> Token {
        let span = Span::new(self.source.clone(), self.previous, self.current);
        Token::new(TokenType::Error(msg.to_string().into_boxed_str()), span)
    }

    fn remaining(&mut self) -> &str {
        &self.source.contents[self.current..]
    }

    fn advance(&mut self) -> Option<&str> {
        if self.remaining().is_empty() {
            None
        } else {
            let source = &self.source.contents[self.current..];
            let mut end = 1;
            while !source.is_char_boundary(end) {
                end += 1;
            }

            self.current += end;

            Some(&source[0..end])
        }
    }

    fn peek(&mut self) -> Option<&str> {
        if self.remaining().is_empty() {
            None
        } else {
            let source = &self.source.contents[self.current..];
            let mut end = 1;
            while !source.is_char_boundary(end) {
                end += 1;
            }

            Some(&source[0..end])
        }
    }

    fn skip_next(&mut self) {
        self.previous = self.current;
    }

    fn match_(&mut self, check: &str) -> bool {
        if self.peek() == Some(check) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn number(&mut self) -> Token {
        while self.peek() != None && is_digit(self.peek().unwrap()) {
            self.advance();
        }

        let string_value = &self.source.contents[self.previous..self.current];
        let parse_value = string_value.parse::<f64>().unwrap();
        self.make_token(TokenType::Number(parse_value))
    }

    fn identifier(&mut self) -> Token {
        while self.peek() != None && is_alpha(self.peek().unwrap()) {
            self.advance();
        }

        let token_type = self.identifier_type();
        self.make_token(token_type)
    }

    fn identifier_type(&mut self) -> TokenType {
        let value = &self.source.contents[self.previous..self.current];
        match &value[..] {
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
            "end" => TokenType::End,
            "loop" => TokenType::Loop,
            "while" => TokenType::While,
            "endloop" => TokenType::EndLoop,
            "break" => TokenType::Break,
            "continue" => TokenType::Continue,
            _ => TokenType::Ident(value.to_string().into_boxed_str()),
        }
    }

    fn single_line_comment(&mut self) -> Token {
        while self.peek() != None && self.peek() != Some("\n") {
            self.advance();
        }

        // incerment previous by two so that the comment message doesn't contain the starting '//'.
        let value = self.source.contents[self.previous + 2..self.current].to_string();
        self.make_token(TokenType::Comment(value.into_boxed_str(), false))
    }

    fn scan_string(&mut self) -> Token {
        // first quote
        self.skip_next();

        while self.peek() != Some("\"") {
            if self.peek() == None {
                return self.make_error_token("Unterminated string");
            }
            self.advance();
        }

        let value = self.source.contents[self.previous..self.current].to_string();
        // closing quote
        self.advance();
        self.skip_next();

        self.make_token(TokenType::String(value.into_boxed_str()))
    }

    fn skip_whitespace(&mut self) -> &mut Self {
        while self.peek() != None && is_whitespace(self.peek().unwrap()) {
            self.advance();
        }
        self.previous = self.current;
        self
    }
}

fn is_whitespace(string: &str) -> bool {
    matches!(
        string,
        // Usual ASCII suspects
        "\u{0009}"   // \t
        | "\u{000B}" // vertical tab
        | "\u{000C}" // form feed
        | "\u{000D}" // \r
        | "\u{0020}" // space

        // NEXT LINE from latin1
        | "\u{0085}"

        // Bidi markers
        | "\u{200E}" // LEFT-TO-RIGHT MARK
        | "\u{200F}" // RIGHT-TO-LEFT MARK

        // Dedicated whitespace characters from Unicode
        | "\u{2028}" // LINE SEPARATOR
        | "\u{2029}" // PARAGRAPH SEPARATOR
    )
}

fn is_alpha(string: &str) -> bool {
    string
        .bytes()
        .all(|b| matches!(b, b'a'..=b'z' | b'A'..=b'Z' | b'_'))
}

fn is_digit(string: &str) -> bool {
    string.as_bytes()[0].is_ascii_digit()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scan_tokens() {
        let tests = vec![
            ("+", TokenType::Plus),
            ("-", TokenType::Minus),
            ("*", TokenType::Star),
            ("/", TokenType::Slash),
            ("!", TokenType::Bang),
            ("=", TokenType::Equals),
            ("<", TokenType::LessThan),
            ("<=", TokenType::LessThanEquals),
            (">", TokenType::GreaterThan),
            (">=", TokenType::GreaterThanEquals),
            ("==", TokenType::EqualsTo),
            ("!=", TokenType::NotEqual),
            ("(", TokenType::LeftParen),
            (")", TokenType::RightParen),
            ("{", TokenType::LeftBrace),
            ("}", TokenType::RightBrace),
            ("true", TokenType::True),
            ("false", TokenType::False),
            ("nil", TokenType::Nil),
            ("var", TokenType::Var),
            ("print", TokenType::Print),
            ("and", TokenType::And),
            ("or", TokenType::Or),
            ("if", TokenType::If),
            ("then", TokenType::Then),
            ("else", TokenType::Else),
            ("end", TokenType::End),
            ("loop", TokenType::Loop),
            ("while", TokenType::While),
            ("endloop", TokenType::EndLoop),
            ("break", TokenType::Break),
            ("continue", TokenType::Continue),
        ];

        for (src, token_type) in tests {
            let source = Source::source(src);
            let mut scanner = Scanner::new(source);

            let token = scanner.scan_token();

            assert_eq!(token.token_type, token_type);
            assert_eq!(token.syntax(), src);
        }
    }

    #[test]
    fn scan_number_token() {
        let tests = vec![("12345", 12345.0)];

        for (val, num) in tests {
            let src = Source::source(val);
            let mut scanner = Scanner::new(src);

            let token = scanner.scan_token();
            assert_eq!(token.token_type, TokenType::Number(num));
            assert_eq!(token.syntax(), val);
        }
    }

    #[test]
    fn skip_whitespace() {
        let tests = vec![
            ("             ", 0),
            ("\r\r\t", 0),
            ("   123 +    45    ", 3),
        ];

        for (snippet, expected_count) in tests {
            let src = Source::source(snippet);
            let mut scanner = Scanner::new(src);

            let mut token_count = 0;
            while scanner.scan_token().token_type != TokenType::Eof {
                token_count += 1;
            }

            assert_eq!(expected_count, token_count);
        }
    }

    #[test]
    fn scan_string_token() {
        let tests = vec![
            ("\"Hello, World!\"", "Hello, World!"),
            ("\"猫\"", "猫"),
            ("\"var\"", "var"),
            ("\"\"", ""),
        ];

        for (string, syntax) in tests {
            let src = Source::source(string);
            let mut scanner = Scanner::new(src);
            let expected = String::from(syntax).into_boxed_str();
            assert_eq!(scanner.scan_token().token_type, TokenType::String(expected));
        }
    }

    #[test]
    fn token_span() {
        let src = Source::source("123 val 猫");
        let spans = vec![(0, 3), (4, 7), (8, 11), (11, 11)];

        let mut scanner = Scanner::new(src);

        for span in spans {
            let token = scanner.scan_token();
            assert_eq!(token.span.start, span.0);
            assert_eq!(token.span.end, span.1);
        }
    }

    #[test]
    fn scan_identifier_token() {
        let src = Source::source("radishes");
        let mut scanner = Scanner::new(src);
        let token = scanner.scan_token();

        let expect = String::from("radishes").into_boxed_str();

        assert_eq!(token.token_type, TokenType::Ident(expect));
        assert_eq!(token.syntax(), "radishes");
    }

    #[test]
    fn scan_newline_token() {
        let src = Source::source("\n");
        let mut scanner = Scanner::new(src);
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Newline);
        assert_eq!(token.syntax(), "\\n");
    }

    #[test]
    fn scan_single_line_comment() {
        let src = Source::source("//this is a comment");
        let mut scanner = Scanner::new(src);
        let expected = String::from("this is a comment").into_boxed_str();
        assert_eq!(
            scanner.scan_token().token_type,
            TokenType::Comment(expected, false)
        );
    }

    #[test]
    fn scan_unexpected_token() {
        let src = Source::source("猫");
        let mut scanner = Scanner::new(src);
        let token = scanner.scan_token();
        assert_eq!(
            token.token_type,
            TokenType::Error(String::from("猫").into_boxed_str())
        );
        assert_eq!(token.syntax(), "猫");
    }

    #[test]
    fn scan_empty_file() {
        let src = Source::source("");
        let mut scanner = Scanner::new(src);
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Eof);
        assert_eq!(token.syntax(), "<Eof>");
    }
}
