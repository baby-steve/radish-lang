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
            Some("+") => {
                if self.match_("=") {
                    self.make_token(TokenType::PlusEquals)
                } else {
                    self.make_token(TokenType::Plus)
                }
            }
            Some("-") => {
                if self.match_("=") {
                    self.make_token(TokenType::MinusEquals)
                } else {
                    self.make_token(TokenType::Minus)
                }
            }
            Some("/") => {
                if self.match_("/") {
                    self.single_line_comment()
                } else if self.match_("=") {
                    self.make_token(TokenType::DivideEquals)
                } else {
                    self.make_token(TokenType::Slash)
                }
            }
            Some("*") => {
                if self.match_("=") {
                    self.make_token(TokenType::MultiplyEquals)
                } else {
                    self.make_token(TokenType::Star)
                }
            }
            Some("%") => {
                if self.match_("=") {
                    self.make_token(TokenType::ModuloEquals)
                } else {
                    self.make_token(TokenType::Percent)
                }
            }
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
            Some(",") => self.make_token(TokenType::Comma),
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

    fn current_token(&mut self) -> Option<&str> {
        if self.remaining().is_empty() {
            None
        } else {
            let source = &self.source.contents[self.previous..];
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
        if self.current_token() == Some("0") {
            match self.peek() {
                Some("b") | Some("B") => {
                    self.advance();

                    while self.peek() != None && self.peek() == Some("0")
                        || self.peek() == Some("1")
                    {
                        self.advance();
                    }

                    let string_value = &self.source.contents[self.previous + 2..self.current];

                    // Todo: if the string fails to parse, should report an error.
                    let parse_value = match isize::from_str_radix(string_value, 2) {
                        Ok(val) => val,
                        Err(err) => panic!("{}", err),
                    };
                    return self.make_token(TokenType::Number(parse_value as f64));
                }
                Some("o") | Some("O") => {
                    self.advance();

                    while self.peek() != None && is_digit(self.peek().unwrap()) {
                        if !is_octal(self.advance().unwrap()) {
                            panic!("Expected digits between 0 and 7");
                        }
                    }

                    let string_value = &self.source.contents[self.previous + 2..self.current];

                    // Todo: if the string fails to parse, should report an error.
                    let parse_value = match isize::from_str_radix(string_value, 8) {
                        Ok(val) => val,
                        Err(err) => panic!("{}", err),
                    };
                    return self.make_token(TokenType::Number(parse_value as f64));
                }
                Some("x") | Some("X") => {
                    self.advance();

                    while self.peek() != None && is_hex(self.peek().unwrap()) {
                        self.advance();
                    }

                    let string_value = &self.source.contents[self.previous + 2..self.current];

                    // Todo: if the string fails to parse, should report an error.
                    let parse_value = match isize::from_str_radix(string_value, 16) {
                        Ok(val) => val,
                        Err(err) => panic!("{}", err),
                    };
                    return self.make_token(TokenType::Number(parse_value as f64));
                }
                _ => {}
            };
        }

        while self.peek() != None && is_digit(self.peek().unwrap()) {
            self.advance();
        }

        // check if this is a floating point number.
        if self.peek() == Some(".") {
            self.advance();
            while self.peek() != None && is_digit(self.peek().unwrap()) {
                self.advance();
            }
        }

        // check if it's in scientific notation.
        if self.peek() == Some("e") || self.peek() == Some("E") {
            self.advance();

            if self.peek() == Some("-") || self.peek() == Some("+") {
                self.advance();
            }

            while self.peek() != None && is_digit(self.peek().unwrap()) {
                self.advance();
            }
        }

        let string_value = &self.source.contents[self.previous..self.current];

        // Todo: if the string fails to parse, should report an error.
        let parse_value = match string_value.parse::<f64>() {
            Ok(val) => val,
            Err(err) => panic!("{}", err),
        };
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
            "endif" => TokenType::EndIf,
            "loop" => TokenType::Loop,
            "while" => TokenType::While,
            "endloop" => TokenType::EndLoop,
            "break" => TokenType::Break,
            "continue" => TokenType::Continue,
            "fun" => TokenType::Fun,
            "return" => TokenType::Return,
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

fn is_octal(string: &str) -> bool {
    // Todo: isn't there a better way to do this?
    matches!(string, "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7")
}

fn is_hex(string: &str) -> bool {
    if is_digit(string)
        || string
            .bytes()
            .all(|b| matches!(b, b'a'..=b'f' | b'A'..=b'F'))
    {
        true
    } else {
        false
    }
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
            ("%", TokenType::Percent),
            ("!", TokenType::Bang),
            ("=", TokenType::Equals),
            ("<", TokenType::LessThan),
            ("<=", TokenType::LessThanEquals),
            (">", TokenType::GreaterThan),
            (">=", TokenType::GreaterThanEquals),
            ("==", TokenType::EqualsTo),
            ("!=", TokenType::NotEqual),
            ("+=", TokenType::PlusEquals),
            ("-=", TokenType::MinusEquals),
            ("*=", TokenType::MultiplyEquals),
            ("/=", TokenType::DivideEquals),
            ("%=", TokenType::ModuloEquals),
            ("(", TokenType::LeftParen),
            (")", TokenType::RightParen),
            ("{", TokenType::LeftBrace),
            ("}", TokenType::RightBrace),
            (",", TokenType::Comma),
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
            ("endif", TokenType::EndIf),
            ("loop", TokenType::Loop),
            ("while", TokenType::While),
            ("endloop", TokenType::EndLoop),
            ("break", TokenType::Break),
            ("continue", TokenType::Continue),
            ("fun", TokenType::Fun),
            ("return", TokenType::Return),
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
        let tests = vec![
            ("12345", 12345.0, "12345"),
            ("23.45", 23.45, "23.45"),
            ("34.", 34.0, "34"),
            ("0.55", 0.55, "0.55"),
            ("23e10", 23e10, "230000000000"),
            ("23e+10", 23e10, "230000000000"),
            ("23e-10", 23e-10, "0.0000000023"),
            ("23.45e5", 23.45e5, "2345000"),
            ("23E10", 23e10, "230000000000"),
            ("0b101101", 45.0, "45"),
            ("0o10", 8.0, "8"),
            ("0x2f", 47.0, "47"),
            ("0x2F", 47.0, "47"),
        ];

        for (val, num, syntax) in tests {
            let src = Source::source(val);
            let mut scanner = Scanner::new(src);

            let token = scanner.scan_token();
            assert_eq!(token.token_type, TokenType::Number(num));
            assert_eq!(token.syntax(), syntax);
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
