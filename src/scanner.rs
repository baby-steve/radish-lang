use unicode_segmentation::UnicodeSegmentation;

use crate::token::{Span, Token, TokenType};

pub struct Scanner<'a> {
    source: Vec<&'a str>,
    current: usize,
    previous: usize,
    line: usize,
}

impl<'a, 'b> Scanner<'a> {
    pub fn new(src: &'a str) -> Scanner<'a> {
        Scanner {
            source: src.graphemes(true).collect::<Vec<&str>>(),
            current: 0,
            previous: 0,
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Token {
        let c = self.skip_whitespace().advance();

        match c {
            Some("+") => self.make_token(TokenType::Plus),
            Some("-") => self.make_token(TokenType::Minus),
            Some("/") => self.make_token(TokenType::Slash),
            Some("*") => self.make_token(TokenType::Star),
            None => self.make_token(TokenType::Eof),
            _ => {
                if is_digit(c.unwrap()) {
                    return self.number();
                } else {
                    let msg = format!("Unexpected character: '{}'", c.unwrap());
                    return self.make_error_token(msg);
                }
            }
        }
    }

    fn advance(&mut self) -> Option<&str> {
        self.current = self.current + 1;

        if self.current > self.source.len() {
            None
        } else {
            Some(self.source[self.current - 1])
        }
    }

    fn peek(&mut self) -> Option<&str> {
        let index = self.current + 1;

        if index > self.source.len() {
            None
        } else {
            Some(self.source[index - 1])
        }
    }

    fn make_token(&mut self, token_type: TokenType) -> Token {
        let token = match self.current {
            _ if self.current > self.source.len() => {
                let span = Span::new(self.line, self.previous);
                Token::new(token_type, "".to_string(), span)
            }
            _ => {
                let value = self.source[self.previous..self.current].join("");
                let span = Span::new(self.line, self.previous);
                self.previous = self.current;

                Token::new(token_type, value, span)
            }
        };

        token
    }

    fn make_error_token(&mut self, msg: String) -> Token {
        let span = Span::new(self.line, self.previous);
        Token::new(TokenType::Error, msg, span)
    }

    fn number(&mut self) -> Token {
        while self.peek() != None && is_digit(self.peek().unwrap()) {
            self.advance();
        }

        self.make_token(TokenType::Number)
    }

    fn skip_whitespace(&mut self) -> &mut Self {
        while self.peek() != None && is_whitespace(self.peek().unwrap()) {
            self.advance();
        }
        self.previous = self.current;
        self
    }
}

fn is_digit(string: &str) -> bool {
    string.as_bytes()[0].is_ascii_digit()
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_init_scanner() {
        let source = String::from("123猫");
        let scanner = Scanner::new(&source);
        assert_eq!(scanner.source, ["1", "2", "3", "猫"]);
    }

    #[test]
    fn test_op_token_type() {
        let src = String::from("+-*/");
        let mut scanner = Scanner::new(&src);
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Plus);
        assert_eq!(token.value, "+");
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Minus);
        assert_eq!(token.value, "-");
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Star);
        assert_eq!(token.value, "*");
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Slash);
        assert_eq!(token.value, "/");
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Eof);
        assert_eq!(token.value, "");
    }

    #[test]
    fn test_number_token_type() {
        let src = String::from("123");
        let mut scanner = Scanner::new(&src);
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Number);
        assert_eq!(token.value, "123");
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Eof);
        assert_eq!(token.value, "");
    }

    #[test]
    fn test_skip_whitespace() {
        let src = String::from("    ");
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.scan_token().token_type, TokenType::Eof);
        let src = String::from("\r\r\t");
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.scan_token().token_type, TokenType::Eof);
        let src = String::from("  123    + 45  ");
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.scan_token().token_type, TokenType::Number);
        assert_eq!(scanner.scan_token().token_type, TokenType::Plus);
        assert_eq!(scanner.scan_token().token_type, TokenType::Number);
        assert_eq!(scanner.scan_token().token_type, TokenType::Eof);
    }

    #[test]
    fn test_unexpected_token_type() {
        let src = String::from("猫");
        let mut scanner = Scanner::new(&src);
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Error);
        assert_eq!(token.value, "Unexpected character: '猫'");
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Eof);
        assert_eq!(token.value, "");
    }
    #[test]
    fn test_multiple_tokens() {
        let src = String::from("1 + 23 + 456");
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.scan_token().token_type, TokenType::Number);
        assert_eq!(scanner.scan_token().token_type, TokenType::Plus);
        assert_eq!(scanner.scan_token().token_type, TokenType::Number);
        assert_eq!(scanner.scan_token().token_type, TokenType::Plus);
        assert_eq!(scanner.scan_token().token_type, TokenType::Number);
        assert_eq!(scanner.scan_token().token_type, TokenType::Eof);
    }

    #[test]
    fn test_token_span() {
        let src = String::from("123 456 猫");
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.source.len(), 9);
        let token = scanner.scan_token(); //123
        assert_eq!(token.span.line, 1);
        assert_eq!(token.span.offset, 0);
        let token = scanner.scan_token(); //456
        assert_eq!(token.span.line, 1);
        assert_eq!(token.span.offset, 4);
        let token = scanner.scan_token(); //猫
        assert_eq!(token.span.line, 1);
        assert_eq!(token.span.offset, 8);
        let token = scanner.scan_token(); //Eof
        assert_eq!(token.span.line, 1);
        assert_eq!(token.span.offset, 9);
    }

    #[test]
    fn test_empty_file() {
        let src = String::from("");
        let mut scanner = Scanner::new(&src);
        let token = scanner.scan_token();
        assert_eq!(token.token_type, TokenType::Eof);
    }
}
