use crate::ast::*;
use crate::scanner::Scanner;
use crate::token::{Token, TokenType};

pub struct Parser<'a> {
    scanner: Scanner<'a>,
    previous: Option<Token>,
    current: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Parser<'a> {
        Parser {
            scanner: Scanner::new(src),
            previous: None,
            current: None,
        }
    }
    pub fn parse(&mut self) -> Result<AST, String> {
        self.advance();

        self.expression()
    }

    fn advance(&mut self) {
        self.previous = self.current.clone();

        loop {
            let token = Some(self.scanner.scan_token());
            if token.clone().unwrap().token_type != TokenType::Error {
                self.current = token;
                break;
            }
            println!("Error: {:?}", token.unwrap().value);
        }
    }

    fn check(&self, token_type: TokenType) -> bool {
        self.current.as_ref().unwrap().token_type == token_type
    }

    fn match_token(&mut self, token_type: TokenType) -> bool {
        if !self.check(token_type) {
            false
        } else {
            self.advance();
            true
        }
    }

    fn consume(&mut self, token_type: TokenType, err_msg: &str) {
        if self.current.as_ref().unwrap().token_type == token_type {
            self.advance();
        } else {
            println!("{}", err_msg);
        }
    }

    fn expression(&mut self) -> Result<AST, String> {
        self.parse_sum()
    }

    fn parse_sum(&mut self) -> Result<AST, String> {
        let mut node = self.parse_term()?;
        loop {
            match self.current.as_ref().unwrap().token_type {
                TokenType::Plus => {
                    self.consume(TokenType::Plus, "Expect '+'");
                    node = AST::BinaryExpr(Box::new(BinaryExpr {
                        left: node,
                        op: Op::Add,
                        right: self.parse_term()?,
                    }));
                }
                TokenType::Minus => {
                    self.consume(TokenType::Minus, "Expect '-'");
                    node = AST::BinaryExpr(Box::new(BinaryExpr {
                        left: node,
                        op: Op::Subtract,
                        right: self.parse_term()?,
                    }))
                }
                _ => break,
            }
        };

        return Ok(node);
    }

    fn parse_term(&mut self) -> Result<AST, String> {
        let mut node = self.parse_factor()?;
        loop {
            match self.current.as_ref().unwrap().token_type {
                TokenType::Star => {
                    self.consume(TokenType::Star, "Expect '*'");
                    node = AST::BinaryExpr(Box::new(BinaryExpr {
                        left: node,
                        op: Op::Multiply,
                        right: self.parse_factor()?,
                    }));
                }
                TokenType::Slash => {
                    self.consume(TokenType::Slash, "Expect '*'");
                    node = AST::BinaryExpr(Box::new(BinaryExpr {
                        left: node,
                        op: Op::Divide,
                        right: self.parse_factor()?,
                    }));
                }
                _ => break,
            };
        };

        Ok(node)
    }

    fn parse_factor(&mut self) -> Result<AST, String> {
        match self.current.as_ref().unwrap().token_type {
            TokenType::Number => {
                let value = self
                    .current
                    .as_ref()
                    .unwrap()
                    .value
                    .parse::<f64>()
                    .expect("Error couldn't parse value");
                let node = AST::Literal(Literal::Number(value));
                self.consume(TokenType::Number, "Expect number literal");
                return Ok(node);
            }
            _ => return Err(String::from("Error unexpected token")),
        }
    }
}
