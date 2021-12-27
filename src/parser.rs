use std::num::ParseFloatError;

use crate::ast::*;
use crate::scanner::Scanner;
use crate::token::{Span, Token, TokenType};

#[derive(Debug)]
pub struct ParserError(String);

impl From<ParseFloatError> for ParserError {
    fn from(_: ParseFloatError) -> Self {
        ParserError("Cannot parse number".to_string())
    }
}

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
    pub fn parse(&mut self) -> Result<AST, ParserError> {
        self.advance();

        Ok(AST::new(vec![self.expression()?]))
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

    fn expression(&mut self) -> Result<ASTNode, ParserError> {
        self.parse_sum()
    }

    fn parse_sum(&mut self) -> Result<ASTNode, ParserError> {
        let mut node = self.parse_term()?;
        loop {
            match self.current.as_ref().unwrap().token_type {
                TokenType::Plus => {
                    self.consume(TokenType::Plus, "Expect '+'");
                    let right = self.parse_term()?;
                    let span = Span::new(node.position().start, right.position().end);
                    node = ASTNode::BinaryExpr(
                        Box::new(BinaryExpr {
                            left: node,
                            op: Op::Add,
                            right,
                        }),
                        span,
                    );
                }
                TokenType::Minus => {
                    self.consume(TokenType::Minus, "Expect '-'");
                    let right = self.parse_term()?;
                    let span = Span::new(node.position().start, right.position().end);
                    node = ASTNode::BinaryExpr(
                        Box::new(BinaryExpr {
                            left: node,
                            op: Op::Subtract,
                            right,
                        }),
                        span,
                    );
                }
                _ => break,
            }
        }

        return Ok(node);
    }

    fn parse_term(&mut self) -> Result<ASTNode, ParserError> {
        let mut node = self.parse_factor()?;
        loop {
            match self.current.as_ref().unwrap().token_type {
                TokenType::Star => {
                    self.consume(TokenType::Star, "Expect '*'");
                    let right = self.parse_factor()?;
                    let span = Span::new(node.position().start, right.position().end);
                    node = ASTNode::BinaryExpr(
                        Box::new(BinaryExpr {
                            left: node,
                            op: Op::Multiply,
                            right,
                        }),
                        span,
                    );
                }
                TokenType::Slash => {
                    self.consume(TokenType::Slash, "Expect '/'");
                    let right = self.parse_factor()?;
                    let span = Span::new(node.position().start, right.position().end);
                    node = ASTNode::BinaryExpr(
                        Box::new(BinaryExpr {
                            left: node,
                            op: Op::Divide,
                            right,
                        }),
                        span,
                    );
                }
                _ => break,
            };
        }

        Ok(node)
    }

    fn parse_factor(&mut self) -> Result<ASTNode, ParserError> {
        match self.current.as_ref().unwrap().token_type {
            TokenType::Number => {
                let current_token = self.current.as_ref().unwrap();
                let value = current_token.value.parse::<f64>()?;
                let node = ASTNode::Literal(Literal::Number(value), current_token.span);
                self.consume(TokenType::Number, "Expect number literal");
                return Ok(node);
            }
            TokenType::LeftParen => {
                let start = self.current.as_ref().unwrap().span.start;
                self.consume(TokenType::LeftParen, "Expect '('.");
                let expr = self.parse_sum()?;
                self.consume(
                    TokenType::RightParen,
                    "Expect ')' after grouping expression.",
                );
                let span = Span::new(start, expr.position().end + 1);
                let node = ASTNode::ParenExpr(Box::new(ParenExpr { expr }), span);
                return Ok(node);
            }
            TokenType::Minus => {
                let start = self.current.as_ref().unwrap().span.start;
                self.consume(TokenType::Minus, "Expect '-'.");
                let op = Op::Subtract;
                let arg = self.parse_factor()?;
                let span = Span::new(start, arg.position().end);
                let node = ASTNode::UnaryExpr(Box::new(UnaryExpr { op, arg }), span);
                return Ok(node);
            }
            _ => {
                return Err(ParserError(String::from(format!(
                    "Error, unexpected token: '{}'.",
                    self.current.as_ref().unwrap().token_type
                ))))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // TODO: implement more through tests.

    #[test]
    fn test_binary_add_expr() {
        let result = &Parser::new("1 + 23").parse().unwrap().items[0];

        let expr = match result {
            ASTNode::BinaryExpr(val, _) => val,
            _ => unreachable!(),
        };

        assert_eq!(
            expr.left,
            ASTNode::Literal(Literal::Number(1.0), Span::new(0, 1))
        );
        assert_eq!(expr.op, Op::Add);
        assert_eq!(
            expr.right,
            ASTNode::Literal(Literal::Number(23.0), Span::new(4, 6))
        );
    }

    #[test]
    fn test_binary_sub_expr() {
        let result = &Parser::new("1 - 23").parse().unwrap().items[0];

        let expr = match result {
            ASTNode::BinaryExpr(val, _) => val,
            _ => unreachable!(),
        };

        assert_eq!(
            expr.left,
            ASTNode::Literal(Literal::Number(1.0), Span::new(0, 1))
        );
        assert_eq!(expr.op, Op::Subtract);
        assert_eq!(
            expr.right,
            ASTNode::Literal(Literal::Number(23.0), Span::new(4, 6))
        );
    }

    #[test]
    fn test_binary_mul_expr() {
        let result = &Parser::new("1 * 23").parse().unwrap().items[0];

        let expr = match result {
            ASTNode::BinaryExpr(val, _) => val,
            _ => unreachable!(),
        };

        assert_eq!(
            expr.left,
            ASTNode::Literal(Literal::Number(1.0), Span::new(0, 1))
        );
        assert_eq!(expr.op, Op::Multiply);
        assert_eq!(
            expr.right,
            ASTNode::Literal(Literal::Number(23.0), Span::new(4, 6))
        );
    }

    #[test]
    fn test_binary_div_expr() {
        let result = &Parser::new("1 / 23").parse().unwrap().items[0];

        let expr = match result {
            ASTNode::BinaryExpr(val, _) => val,
            _ => unreachable!(),
        };

        assert_eq!(
            expr.left,
            ASTNode::Literal(Literal::Number(1.0), Span::new(0, 1))
        );
        assert_eq!(expr.op, Op::Divide);
        assert_eq!(
            expr.right,
            ASTNode::Literal(Literal::Number(23.0), Span::new(4, 6))
        );
    }
}
