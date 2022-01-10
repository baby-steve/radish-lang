use std::rc::Rc;

use crate::source::Source;
use crate::span::Span;
use crate::token::{Token, TokenType};

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
        };

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

    fn new_test_scanner(test_string: &str) -> Scanner {
        let source = Source::source(test_string);
        Scanner::new(source)
    }

    #[test]
    fn test_op_token_type() {
        let src = String::from("+-*/");
        let mut scanner = new_test_scanner(&src);
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Plus);
        assert_eq!(token.syntax(), "+");
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Minus);
        assert_eq!(token.syntax(), "-");
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Star);
        assert_eq!(token.syntax(), "*");
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Slash);
        assert_eq!(token.syntax(), "/");
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Eof);
        assert_eq!(token.syntax(), "<Eof>");
    }

    #[test]
    fn test_comparison_op_token() {
        let mut scanner = new_test_scanner("< > = <= >= == !=");

        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::LessThan);
        assert_eq!(token.syntax(), "<");

        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::GreaterThan);
        assert_eq!(token.syntax(), ">");

        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Equals);
        assert_eq!(token.syntax(), "=");

        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::LessThanEquals);
        assert_eq!(token.syntax(), "<=");

        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::GreaterThanEquals);
        assert_eq!(token.syntax(), ">=");

        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::EqualsTo);
        assert_eq!(token.syntax(), "==");

        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::NotEqual);
        assert_eq!(token.syntax(), "!=");
    }

    #[test]
    fn test_number_token_type() {
        let src = String::from("123");
        let mut scanner = new_test_scanner(&src);
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Number(123.0));
        assert_eq!(token.syntax(), "123");
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Eof);
        assert_eq!(token.syntax(), "<Eof>");
    }

    #[test]
    fn test_true_token() {
        let mut scanner = new_test_scanner("true");
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::True);
        assert_eq!(token.syntax(), "true");
    }

    #[test]
    fn test_false_token() {
        let mut scanner = new_test_scanner("false");
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::False);
        assert_eq!(token.syntax(), "false");
    }

    #[test]
    fn test_identifier_token() {
        let mut scanner = new_test_scanner("radishes cats");
        let token = scanner.scan_token();
        assert_eq!(
            token.token_type,
            TokenType::Ident(String::from("radishes").into_boxed_str())
        );
        assert_eq!(token.syntax(), "radishes");
        let token = scanner.scan_token();
        assert_eq!(
            token.token_type,
            TokenType::Ident(String::from("cats").into_boxed_str())
        );
        assert_eq!(token.syntax(), "cats");
    }

    #[test]
    fn test_newline_token() {
        let mut scanner = new_test_scanner("123\n456");
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Number(123.0));
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Newline);
        assert_eq!(token.syntax(), "\\n");
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Number(456.0));
    }

    #[test]
    fn test_single_line_comment() {
        let mut scanner = new_test_scanner("//this is a comment");
        assert_eq!(
            scanner.scan_token().token_type,
            TokenType::Comment("this is a comment".to_string().into_boxed_str(), false)
        );
    }

    #[test]
    fn test_nil_token() {
        let mut scanner = new_test_scanner("nil");
        assert_eq!(
            scanner.scan_token().token_type,
            TokenType::Nil,
        );
    }

    #[test]
    fn test_bang_token() {
        let mut scanner = new_test_scanner("!");
        assert_eq!(scanner.scan_token().token_type, TokenType::Bang);
    }

    #[test]
    fn test_var_token() {
        let mut scanner = new_test_scanner("var");
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Var);
        assert_eq!(token.syntax(), "var");
    }

    #[test]
    fn test_skip_whitespace() {
        let src = String::from("    ");
        let mut scanner = new_test_scanner(&src);
        assert_eq!(scanner.scan_token().token_type, TokenType::Eof);
        let src = String::from("\r\r\t");
        let mut scanner = new_test_scanner(&src);
        assert_eq!(scanner.scan_token().token_type, TokenType::Eof);
        let src = String::from("  123    + 45  ");
        let mut scanner = new_test_scanner(&src);
        assert_eq!(scanner.scan_token().token_type, TokenType::Number(123.0));
        assert_eq!(scanner.scan_token().token_type, TokenType::Plus);
        assert_eq!(scanner.scan_token().token_type, TokenType::Number(45.0));
        assert_eq!(scanner.scan_token().token_type, TokenType::Eof);
    }

    #[test]
    fn test_unexpected_token_type() {
        let src = String::from("猫");
        let mut scanner = new_test_scanner(&src);
        let token = scanner.scan_token();
        assert_eq!(
            token.token_type,
            TokenType::Error(String::from("猫").into_boxed_str())
        );
        assert_eq!(token.syntax(), "猫");
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Eof);
        assert_eq!(token.syntax(), "<Eof>");
    }
    #[test]
    fn test_multiple_tokens() {
        let src = String::from("1 + 23 + 456");
        let mut scanner = new_test_scanner(&src);
        assert_eq!(scanner.scan_token().token_type, TokenType::Number(1.0));
        assert_eq!(scanner.scan_token().token_type, TokenType::Plus);
        assert_eq!(scanner.scan_token().token_type, TokenType::Number(23.0));
        assert_eq!(scanner.scan_token().token_type, TokenType::Plus);
        assert_eq!(scanner.scan_token().token_type, TokenType::Number(456.0));
        assert_eq!(scanner.scan_token().token_type, TokenType::Eof);
    }

    #[test]
    fn test_string_token() {
        let src = "2 \"Hello, World!\" 3";
        let mut scanner = new_test_scanner(src);
        scanner.scan_token();
        assert_eq!(
            scanner.scan_token().token_type,
            TokenType::String("Hello, World!".to_string().into_boxed_str())
        );
        assert_eq!(scanner.scan_token().token_type, TokenType::Number(3.0));

        let src = "2 \"\" 3";
        let mut scanner = new_test_scanner(src);
        scanner.scan_token();
        assert_eq!(
            scanner.scan_token().token_type,
            TokenType::String("".to_string().into_boxed_str())
        );
        assert_eq!(scanner.scan_token().token_type, TokenType::Number(3.0));
    }

    #[test]
    fn test_parentheses() {
        let mut scanner = new_test_scanner("123 (456 789)");
        assert_eq!(scanner.scan_token().token_type, TokenType::Number(123.0));
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::LeftParen);
        assert_eq!(token.syntax(), "(");
        assert_eq!(scanner.scan_token().token_type, TokenType::Number(456.0));
        assert_eq!(scanner.scan_token().token_type, TokenType::Number(789.0));
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::RightParen);
        assert_eq!(token.syntax(), ")");
        assert_eq!(scanner.scan_token().token_type, TokenType::Eof);
    }

    #[test]
    fn test_token_span() {
        let src = String::from("789 102 猫");
        //                      0123456789
        let mut scanner = new_test_scanner(&src);
        let token = scanner.scan_token(); //123
        println!("{}", token);
        assert_eq!(token.span.start, 0);
        assert_eq!(token.span.end, 3);
        let token = scanner.scan_token(); //456
        assert_eq!(token.span.start, 4);
        assert_eq!(token.span.end, 7);
        let token = scanner.scan_token(); //猫
        assert_eq!(token.span.start, 8);
        assert_eq!(token.span.end, 11);
        let token = scanner.scan_token(); //Eof
        assert_eq!(token.span.start, 11);
        assert_eq!(token.span.end, 11);
    }

    #[test]
    fn test_empty_file() {
        let src = String::from("");
        let mut scanner = new_test_scanner(&src);
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Eof);
    }

    #[test]
    fn test_is_alpha() {
        assert_eq!(is_alpha("l"), true);
        assert_eq!(is_alpha("L"), true);
        assert_eq!(is_alpha("_"), true);
        assert_eq!(is_alpha("1"), false);
        assert_eq!(is_alpha("?"), false);
        assert_eq!(is_alpha("猫"), false);
    }
}
