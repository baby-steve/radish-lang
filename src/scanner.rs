use unicode_segmentation::UnicodeSegmentation;

use crate::token::{Token, TokenType, Span, LineColumn};

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
        let mut end = self.current;
        
        if self.current > self.source.len() {
            end = self.source.len();
        }
        
        let value = self.source[self.previous..end].join("");
        let span = Span::new(LineColumn::new(self.line, self.previous), LineColumn::new(self.line, end));

        self.previous = self.current;

        Token::new(token_type, value, span)
    }

    fn make_error_token(&mut self, msg: String) -> Token {
        let span = Span::new(LineColumn::new(self.line, self.previous), LineColumn::new(self.line, self.current - 1));
        Token::new(TokenType::Error, msg, span)
    }

    fn number(&mut self) -> Token {
        while self.peek() != None && is_digit(self.peek().unwrap()) {
            self.advance();
        }

        self.make_token(TokenType::Number)
    }

    fn skip_whitespace(&mut self) -> &mut Self {
        while self.peek() != None && self.peek() == Some(" ") {
            self.advance();
        }
        self.previous = self.current;
        self
    }
}

fn is_digit(string: &str) -> bool {
    string.as_bytes()[0].is_ascii_digit()
}

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
}
