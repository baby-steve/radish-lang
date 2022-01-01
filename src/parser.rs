use std::rc::Rc;

use crate::ast::*;
use crate::error::{
    ExpectedError::*, ParserError, SyntaxError, SyntaxError::*, UnexpectedError::*,
};
use crate::scanner::Scanner;
use crate::source::Source;
use crate::span::Span;
use crate::token::{Token, TokenType};

pub struct Parser {
    source: Rc<Source>,
    scanner: Scanner,
    previous: Option<Token>,
    current: Option<Token>,
}

impl Parser {
    pub fn new(source: Rc<Source>) -> Parser {
        Parser {
            source: Rc::clone(&source),
            scanner: Scanner::new(source),
            previous: None,
            current: None,
        }
    }
    pub fn parse(&mut self) -> Result<AST, ParserError> {
        self.advance();

        match self.parse_body() {
            Ok(items) => Ok(AST::new(items)),
            Err(err) => Err(err),
        }
    }

    fn make_error(&mut self, err: ParserError, target_err: SyntaxError) -> ParserError {
        match err.error {
            SyntaxError::ExpectedError(_) => err,
            SyntaxError::UnexpectedError(_) => ParserError::new(target_err, &err.span),
        }
    }

    fn advance(&mut self) {
        self.previous = self.current.clone();

        self.current = Some(self.scanner.scan_token());
    }

    fn check(&self, token_type: &TokenType) -> bool {
        self.current.as_ref().unwrap().token_type == *token_type
    }

    fn match_token(&mut self, token_type: &TokenType) -> bool {
        if !self.check(token_type) {
            false
        } else {
            self.advance();
            true
        }
    }

    fn consume(&mut self, token_type: TokenType) {
        if !self.match_token(&token_type) {
            unreachable!(
                "Unexpected token '{}', expected '{}' at {:?}.",
                self.current.as_ref().unwrap().syntax(),
                token_type,
                self.current.as_ref().unwrap().span,
            );
        }
    }

    fn parse_body(&mut self) -> Result<Vec<ASTNode>, ParserError> {
        let mut items = vec![];

        loop {
            match self.current.as_ref().unwrap().token_type {
                // <Eof>
                TokenType::Eof => {
                    self.advance();
                    return Ok(items);
                }
                // \n or //...
                TokenType::Newline
                | TokenType::Comment(_, false) => {
                    // should single-line comments be parsed?
                    self.advance();
                    continue
                }
                // ...
                _ => items.push(self.parse_statement()?),
            } 
        }
    }

    fn parse_statement(&mut self) -> Result<ASTNode, ParserError> {
        self.expression_statement()
    }

    fn expression_statement(&mut self) -> Result<ASTNode, ParserError> {
        let expr = self.expression()?;

        let span = Span::from(&expr.position());

        Ok(ASTNode::from(Stmt::ExpressionStmt(
            Box::new(ExpressionStmt {expr }),
            span
        )))
    }

    fn expression(&mut self) -> Result<ASTNode, ParserError> {
        self.parse_boolean_factor()
    }

    fn parse_boolean_factor(&mut self) -> Result<ASTNode, ParserError> {
        let mut node = self.parse_sum()?;

        loop {
            match self.current.as_ref().unwrap().token_type {
                // expr < ...
                TokenType::LessThan => {
                    self.consume(TokenType::LessThan);

                    let right = match self.parse_sum() {
                        // expr < expr
                        Ok(expr) => expr,
                        // expr < <error>
                        Err(err) => {
                            return Err(self.make_error(
                                err.clone(),
                                ExpectedError(ExpectedExpression(err.error.to_string())),
                            ))
                        }
                    };

                    let span = Span::combine(&node.position(), &right.position());
                    node = ASTNode::from(Expr::BinaryExpr(
                        Box::new(BinaryExpr {
                            right,
                            left: node,
                            op: Op::LessThan,
                        }),
                        span,
                    ))
                }
                // expr <= ...
                TokenType::LessThanEquals => {
                    self.consume(TokenType::LessThanEquals);

                    let right = match self.parse_sum() {
                        // expr <= expr
                        Ok(expr) => expr,
                        // expr <= <error>
                        Err(err) => {
                            return Err(self.make_error(
                                err.clone(),
                                ExpectedError(ExpectedExpression(err.error.to_string())),
                            ))
                        }
                    };

                    let span = Span::combine(&node.position(), &right.position());
                    node = ASTNode::from(Expr::BinaryExpr(
                        Box::new(BinaryExpr {
                            right,
                            left: node,
                            op: Op::LessThanEquals,
                        }),
                        span,
                    ))
                }
                // expr > ...
                TokenType::GreaterThan => {
                    self.consume(TokenType::GreaterThan);

                    let right = match self.parse_sum() {
                        // expr > expr
                        Ok(expr) => expr,
                        // expr > <error>
                        Err(err) => {
                            return Err(self.make_error(
                                err.clone(),
                                ExpectedError(ExpectedExpression(err.error.to_string())),
                            ))
                        }
                    };

                    let span = Span::combine(&node.position(), &right.position());
                    node = ASTNode::from(Expr::BinaryExpr(
                        Box::new(BinaryExpr {
                            right,
                            left: node,
                            op: Op::GreaterThan,
                        }),
                        span,
                    ))
                }
                // expr >= ...
                TokenType::GreaterThanEquals => {
                    self.consume(TokenType::GreaterThanEquals);

                    let right = match self.parse_sum() {
                        // expr >= expr
                        Ok(expr) => expr,
                        // expr >= <error>
                        Err(err) => {
                            return Err(self.make_error(
                                err.clone(),
                                ExpectedError(ExpectedExpression(err.error.to_string())),
                            ))
                        }
                    };

                    let span = Span::combine(&node.position(), &right.position());
                    node = ASTNode::from(Expr::BinaryExpr(
                        Box::new(BinaryExpr {
                            right,
                            left: node,
                            op: Op::GreaterThanEquals,
                        }),
                        span,
                    ))
                }
                // expr == ...
                TokenType::EqualsTo => {
                    self.consume(TokenType::EqualsTo);

                    let right = match self.parse_sum() {
                        // expr == expr
                        Ok(expr) => expr,
                        // expr == <error>
                        Err(err) => {
                            return Err(self.make_error(
                                err.clone(),
                                ExpectedError(ExpectedExpression(err.error.to_string())),
                            ))
                        }
                    };

                    let span = Span::combine(&node.position(), &right.position());
                    node = ASTNode::from(Expr::BinaryExpr(
                        Box::new(BinaryExpr {
                            right,
                            left: node,
                            op: Op::EqualsTo,
                        }),
                        span,
                    ))
                }
                // expr != ...
                TokenType::NotEqual => {
                    self.consume(TokenType::NotEqual);

                    let right = match self.parse_sum() {
                        // expr != expr
                        Ok(expr) => expr,
                        // expr != <error>
                        Err(err) => {
                            return Err(self.make_error(
                                err.clone(),
                                ExpectedError(ExpectedExpression(err.error.to_string())),
                            ))
                        }
                    };

                    let span = Span::combine(&node.position(), &right.position());
                    node = ASTNode::from(Expr::BinaryExpr(
                        Box::new(BinaryExpr {
                            right,
                            left: node,
                            op: Op::NotEqual,
                        }),
                        span,
                    ))
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn parse_sum(&mut self) -> Result<ASTNode, ParserError> {
        // expr ...
        let mut node = self.parse_term()?;

        loop {
            match self.current.as_ref().unwrap().token_type {
                // expr + ...
                TokenType::Plus => {
                    self.consume(TokenType::Plus);

                    let right = match self.parse_term() {
                        // expr + expr
                        Ok(expr) => expr,
                        // expr + <error>
                        Err(err) => {
                            return Err(self.make_error(
                                err.clone(),
                                ExpectedError(ExpectedExpression(err.error.to_string())),
                            ))
                        }
                    };

                    let span = Span::combine(&node.position(), &right.position());
                    node = ASTNode::from(Expr::BinaryExpr(
                        Box::new(BinaryExpr {
                            right,
                            left: node,
                            op: Op::Add,
                        }),
                        span,
                    ))
                }
                // expr - ...
                TokenType::Minus => {
                    self.consume(TokenType::Minus);

                    let right = match self.parse_term() {
                        // expr - expr
                        Ok(expr) => expr,
                        // expr - <error>
                        Err(err) => {
                            return Err(self.make_error(
                                err.clone(),
                                ExpectedError(ExpectedExpression(err.error.to_string())),
                            ))
                        }
                    };

                    let span = Span::combine(&node.position(), &right.position());

                    node = ASTNode::from(Expr::BinaryExpr(
                        Box::new(BinaryExpr {
                            right,
                            left: node,
                            op: Op::Subtract,
                        }),
                        span,
                    ))
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn parse_term(&mut self) -> Result<ASTNode, ParserError> {
        // expr ...
        let mut node = self.parse_factor()?;

        loop {
            match self.current.as_ref().unwrap().token_type {
                // expr * ...
                TokenType::Star => {
                    self.consume(TokenType::Star);

                    let right = match self.parse_factor() {
                        // expr * expr
                        Ok(expr) => expr,
                        // expr * <error>
                        Err(err) => {
                            return Err(self.make_error(
                                err.clone(),
                                ExpectedError(ExpectedExpression(err.error.to_string())),
                            ))
                        }
                    };

                    let span = Span::combine(&node.position(), &right.position());
                    node = ASTNode::from(Expr::BinaryExpr(
                        Box::new(BinaryExpr {
                            right,
                            left: node,
                            op: Op::Multiply,
                        }),
                        span,
                    ))
                }
                // expr / ...
                TokenType::Slash => {
                    self.consume(TokenType::Slash);

                    let right = match self.parse_factor() {
                        // expr / expr
                        Ok(expr) => expr,
                        // expr / <error>
                        Err(err) => {
                            return Err(self.make_error(
                                err.clone(),
                                ExpectedError(ExpectedExpression(err.error.to_string())),
                            ))
                        }
                    };

                    let span = Span::combine(&node.position(), &right.position());
                    node = ASTNode::from(Expr::BinaryExpr(
                        Box::new(BinaryExpr {
                            right,
                            left: node,
                            op: Op::Divide,
                        }),
                        span,
                    ))
                }
                _ => break,
            };
        }

        Ok(node)
    }

    fn parse_factor(&mut self) -> Result<ASTNode, ParserError> {
        use TokenType::*;

        loop {
            let current = self.current.as_ref().unwrap().clone();

            match current.token_type {
                // <number>
                Number(val) => {
                    let span = Span::from(&current.span);
    
                    let node = ASTNode::from(Expr::Literal(Literal::Number(val), span));
                    self.consume(TokenType::Number(val));
    
                    return Ok(node);
                }
                // ( ...
                LeftParen => return self.parse_paren(),
                // - ...
                Minus => {
                    self.consume(TokenType::Minus);
                    let op = Op::Subtract;
                    let arg = self.parse_factor()?;
    
                    let span = Span::combine(&current.span, &arg.position());
    
                    let node = ASTNode::from(Expr::UnaryExpr(Box::new(UnaryExpr { arg, op }), span));
                    return Ok(node);
                }
                // ! ...
                Bang => {
                    self.consume(TokenType::Bang);
                    let op = Op::Bang;
                    let arg = self.parse_factor()?;

                    let span = Span::combine(&current.span, &arg.position());

                    let node = ASTNode::from(Expr::UnaryExpr(Box::new(UnaryExpr {arg, op }), span));
                    return Ok(node);
                }
                // "true"
                True => {
                    let span = Span::from(&current.span);
                    let node = ASTNode::from(Expr::Literal(Literal::Bool(true), span));
                    self.consume(TokenType::True);
                    return Ok(node);
                }
                // "false"
                False => {
                    let span = Span::from(&current.span);
                    let node = ASTNode::from(Expr::Literal(Literal::Bool(false), span));
                    self.consume(TokenType::False);
                    return Ok(node);
                }
                // <eof>
                Eof => {
                    // if an <eof> token is found here, it has to be an error.
                    let span = &current.span;
                    return Err(ParserError::new(
                        UnexpectedError(UnexpectedEOF),
                        &Span::new(Rc::clone(&self.source), span.start, span.end + 1),
                    ));
                } 
                // single-line comment
                Comment(_, false) => {
                    self.advance();
                    continue;
                }
                // \n
                Newline => {
                    self.advance();
                    continue;
                }
                // lexer error
                _ => {
                    return Err(ParserError::new(
                        UnexpectedError(UnexpectedToken(current.syntax().to_string())),
                        &current.span,
                    ));
                }
            }
        }
    }

    fn parse_paren(&mut self) -> Result<ASTNode, ParserError> {
        let start = self.current.as_ref().unwrap().span.clone();

        // ( ...
        self.consume(TokenType::LeftParen);

        let expr = match self.parse_sum() {
            Ok(expr) => expr,
            Err(err) => {
                return Err(self.make_error(
                    err.clone(),
                    ExpectedError(ExpectedExpression(err.error.to_string())),
                ))
            }
        };

        // ( expr <error>
        if !self.match_token(&TokenType::RightParen) {
            let token = self.current.as_ref().unwrap();

            if token.token_type == TokenType::Eof {
                return Err(ParserError::new(
                    ExpectedError(ExpectedRightParen(format!("found '<Eof>'."))),
                    &Span::new(
                        Rc::clone(&token.span.source),
                        token.span.start,
                        token.span.end + 1,
                    ),
                ));
            }

            return Err(ParserError::new(
                ExpectedError(ExpectedRightParen(format!(
                    "found unexpected token '{}'.",
                    token.syntax()
                ))),
                &Span::from(&token.span),
            ));
        }

        let span = Span::new(
            Rc::clone(&self.source),
            start.start,
            expr.position().end + 1,
        );
        let node = ASTNode::from(Expr::ParenExpr(Box::new(ParenExpr { expr }), span));
        Ok(node)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_boolean_literal() {
        let source = Source::source("true");
        let result = &Parser::new(Rc::clone(&source)).parse().unwrap().items[0];

        assert_eq!(
            *result,
            ASTNode::Stmt(Stmt::ExpressionStmt(
                Box::new(ExpressionStmt {
                    expr: ASTNode::Expr(Expr::Literal(
                        Literal::Bool(true),
                        Span::new(Rc::clone(&source), 0, 4)
                    ),)
                }),
                Span::new(Rc::clone(&source), 0, 4)
            ))
        );

        let source = Source::source("false");
        let result = &Parser::new(Rc::clone(&source)).parse().unwrap().items[0];

        assert_eq!(
            *result,
            ASTNode::Stmt(Stmt::ExpressionStmt(
                Box::new(ExpressionStmt {
                    expr: ASTNode::Expr(Expr::Literal(
                        Literal::Bool(false),
                        Span::new(Rc::clone(&source), 0, 5)
                    ),)
                }),
                Span::new(Rc::clone(&source), 0, 5)
            ))
        )
    }

    #[test]
    fn parse_unary_minus() {
        let source = Source::source("-23");
        let result = &Parser::new(Rc::clone(&source)).parse().unwrap().items[0];

        assert_eq!(
            *result,
            ASTNode::Stmt(Stmt::ExpressionStmt(
                Box::new(ExpressionStmt {
                    expr: ASTNode::Expr(Expr::UnaryExpr(
                        Box::new(UnaryExpr {
                            op: Op::Subtract,
                            arg: ASTNode::Expr(Expr::Literal(
                                Literal::Number(23.0),
                                Span::new(Rc::clone(&source), 1, 3)
                            ))
                        }),
                        Span::new(Rc::clone(&source), 0, 3)
                    ),)
                }),
                Span::new(Rc::clone(&source), 0, 3)
            ))
        )
    }

    #[test]
    fn parse_unary_not() {
        let source = Source::source("!23");
        let result = &Parser::new(Rc::clone(&source)).parse().unwrap().items[0];

        assert_eq!(
            *result,
            ASTNode::Stmt(Stmt::ExpressionStmt(
                Box::new(ExpressionStmt {
                    expr: ASTNode::Expr(Expr::UnaryExpr(
                        Box::new(UnaryExpr {
                            op: Op::Bang,
                            arg: ASTNode::Expr(Expr::Literal(
                                Literal::Number(23.0),
                                Span::new(Rc::clone(&source), 1, 3)
                            ))
                        }),
                        Span::new(Rc::clone(&source), 0, 3)
                    ),)
                }),
                Span::new(Rc::clone(&source), 0, 3)
            ))
        )
    }

    #[test]
    fn parse_binary_add_expr() {
        let source = Source::source("1 + 23");
        let result = &Parser::new(Rc::clone(&source)).parse().unwrap().items[0];

        assert_eq!(
            *result,
            ASTNode::Stmt(Stmt::ExpressionStmt(
                Box::new(ExpressionStmt {
                    expr: ASTNode::Expr(Expr::BinaryExpr(
                        Box::new(BinaryExpr {
                            left: ASTNode::Expr(Expr::Literal(
                                Literal::Number(1.0),
                                Span::new(Rc::clone(&source), 0, 1),
                            )),
                            op: Op::Add,
                            right: ASTNode::Expr(Expr::Literal(
                                Literal::Number(23.0),
                                Span::new(Rc::clone(&source), 4, 6),
                            )),
                        },),
                        Span::new(Rc::clone(&source), 0, 6),
                    ),)
                }),
                Span::new(Rc::clone(&source), 0, 6)
            ))
        )
    }

    #[test]
    fn parse_binary_sub_expr() {
        let source = Source::source("1 - 23");
        let result = &Parser::new(Rc::clone(&source)).parse().unwrap().items[0];
        assert_eq!(
            *result,
            ASTNode::Stmt(Stmt::ExpressionStmt(
                Box::new(ExpressionStmt {
                    expr: ASTNode::Expr(Expr::BinaryExpr(
                        Box::new(BinaryExpr {
                            left: ASTNode::Expr(Expr::Literal(
                                Literal::Number(1.0),
                                Span::new(Rc::clone(&source), 0, 1),
                            )),
                            op: Op::Subtract,
                            right: ASTNode::Expr(Expr::Literal(
                                Literal::Number(23.0),
                                Span::new(Rc::clone(&source), 4, 6),
                            )),
                        },),
                        Span::new(Rc::clone(&source), 0, 6),
                    ),)
                }),
                Span::new(Rc::clone(&source), 0, 6)
            ))
        )
    }

    #[test]
    fn parse_binary_mul_expr() {
        let source = Source::source("1 * 23");
        let result = &Parser::new(Rc::clone(&source)).parse().unwrap().items[0];
        assert_eq!(
            *result,
            ASTNode::Stmt(Stmt::ExpressionStmt(
                Box::new(ExpressionStmt {
                    expr: ASTNode::Expr(Expr::BinaryExpr(
                        Box::new(BinaryExpr {
                            left: ASTNode::Expr(Expr::Literal(
                                Literal::Number(1.0),
                                Span::new(Rc::clone(&source), 0, 1),
                            )),
                            op: Op::Multiply,
                            right: ASTNode::Expr(Expr::Literal(
                                Literal::Number(23.0),
                                Span::new(Rc::clone(&source), 4, 6),
                            )),
                        },),
                        Span::new(Rc::clone(&source), 0, 6),
                    ),)
                }),
                Span::new(Rc::clone(&source), 0, 6)
            ))
        )
    }

    #[test]
    fn parse_binary_div_expr() {
        let source = Source::source("1 / 23");
        let result = &Parser::new(Rc::clone(&source)).parse().unwrap().items[0];
        assert_eq!(
            *result,
            ASTNode::Stmt(Stmt::ExpressionStmt(
                Box::new(ExpressionStmt {
                    expr: ASTNode::Expr(Expr::BinaryExpr(
                        Box::new(BinaryExpr {
                            left: ASTNode::Expr(Expr::Literal(
                                Literal::Number(1.0),
                                Span::new(Rc::clone(&source), 0, 1),
                            )),
                            op: Op::Divide,
                            right: ASTNode::Expr(Expr::Literal(
                                Literal::Number(23.0),
                                Span::new(Rc::clone(&source), 4, 6),
                            )),
                        },),
                        Span::new(Rc::clone(&source), 0, 6),
                    ),)
                }),
                Span::new(Rc::clone(&source), 0, 6)
            ))
        )
    }

    #[test]
    fn parse_less_than_expr() {
        let source = Source::source("2 < 4");
        let result = &Parser::new(Rc::clone(&source)).parse().unwrap().items[0];

        assert_eq!(
            *result,
            ASTNode::Stmt(Stmt::ExpressionStmt(
                Box::new(ExpressionStmt {
                    expr: ASTNode::Expr(Expr::BinaryExpr(
                        Box::new(BinaryExpr {
                            left: ASTNode::Expr(Expr::Literal(
                                Literal::Number(2.0),
                                Span::new(Rc::clone(&source), 0, 1),
                            )),
                            op: Op::LessThan,
                            right: ASTNode::Expr(Expr::Literal(
                                Literal::Number(4.0),
                                Span::new(Rc::clone(&source), 4, 5),
                            )),
                        },),
                        Span::new(Rc::clone(&source), 0, 5),
                    ),)
                }),
                Span::new(Rc::clone(&source), 0, 5)
            ))
        )
    }

    #[test]
    fn parse_less_than_equals_expr() {
        let source = Source::source("2 <= 4");
        let result = &Parser::new(Rc::clone(&source)).parse().unwrap().items[0];

        assert_eq!(
            *result,
            ASTNode::Stmt(Stmt::ExpressionStmt(
                Box::new(ExpressionStmt {
                    expr: ASTNode::Expr(Expr::BinaryExpr(
                        Box::new(BinaryExpr {
                            left: ASTNode::Expr(Expr::Literal(
                                Literal::Number(2.0),
                                Span::new(Rc::clone(&source), 0, 1),
                            )),
                            op: Op::LessThanEquals,
                            right: ASTNode::Expr(Expr::Literal(
                                Literal::Number(4.0),
                                Span::new(Rc::clone(&source), 5, 6),
                            )),
                        },),
                        Span::new(Rc::clone(&source), 0, 6),
                    ),)
                }),
                Span::new(Rc::clone(&source), 0, 6)
            ))
        )
    }

    #[test]
    fn parse_greater_than_expr() {
        let source = Source::source("2 > 4");
        let result = &Parser::new(Rc::clone(&source)).parse().unwrap().items[0];

        assert_eq!(
            *result,
            ASTNode::Stmt(Stmt::ExpressionStmt(
                Box::new(ExpressionStmt {
                    expr: ASTNode::Expr(Expr::BinaryExpr(
                        Box::new(BinaryExpr {
                            left: ASTNode::Expr(Expr::Literal(
                                Literal::Number(2.0),
                                Span::new(Rc::clone(&source), 0, 1),
                            )),
                            op: Op::GreaterThan,
                            right: ASTNode::Expr(Expr::Literal(
                                Literal::Number(4.0),
                                Span::new(Rc::clone(&source), 4, 5),
                            )),
                        },),
                        Span::new(Rc::clone(&source), 0, 5),
                    ),)
                }),
                Span::new(Rc::clone(&source), 0, 5)
            ))
        )
    }

    #[test]
    fn parse_greater_than_equals_expr() {
        let source = Source::source("2 >= 4");
        let result = &Parser::new(Rc::clone(&source)).parse().unwrap().items[0];

        assert_eq!(
            *result,
            ASTNode::Stmt(Stmt::ExpressionStmt(
                Box::new(ExpressionStmt {
                    expr: ASTNode::Expr(Expr::BinaryExpr(
                        Box::new(BinaryExpr {
                            left: ASTNode::Expr(Expr::Literal(
                                Literal::Number(2.0),
                                Span::new(Rc::clone(&source), 0, 1),
                            )),
                            op: Op::GreaterThanEquals,
                            right: ASTNode::Expr(Expr::Literal(
                                Literal::Number(4.0),
                                Span::new(Rc::clone(&source), 5, 6),
                            )),
                        },),
                        Span::new(Rc::clone(&source), 0, 6),
                    ),)
                }),
                Span::new(Rc::clone(&source), 0, 6)
            ))
        )
    }

    #[test]
    fn parse_equals_to_expr() {
        let source = Source::source("2 == 4");
        let result = &Parser::new(Rc::clone(&source)).parse().unwrap().items[0];

        assert_eq!(
            *result,
            ASTNode::Stmt(Stmt::ExpressionStmt(
                Box::new(ExpressionStmt {
                    expr: ASTNode::Expr(Expr::BinaryExpr(
                        Box::new(BinaryExpr {
                            left: ASTNode::Expr(Expr::Literal(
                                Literal::Number(2.0),
                                Span::new(Rc::clone(&source), 0, 1),
                            )),
                            op: Op::EqualsTo,
                            right: ASTNode::Expr(Expr::Literal(
                                Literal::Number(4.0),
                                Span::new(Rc::clone(&source), 5, 6),
                            )),
                        },),
                        Span::new(Rc::clone(&source), 0, 6),
                    ),)
                }),
                Span::new(Rc::clone(&source), 0, 6)
            ))
        )
    }

    #[test]
    fn parse_not_equal_expr() {
        let source = Source::source("2 != 4");
        let result = &Parser::new(Rc::clone(&source)).parse().unwrap().items[0];

        assert_eq!(
            *result,
            ASTNode::Stmt(Stmt::ExpressionStmt(
                Box::new(ExpressionStmt {
                    expr: ASTNode::Expr(Expr::BinaryExpr(
                        Box::new(BinaryExpr {
                            left: ASTNode::Expr(Expr::Literal(
                                Literal::Number(2.0),
                                Span::new(Rc::clone(&source), 0, 1),
                            )),
                            op: Op::NotEqual,
                            right: ASTNode::Expr(Expr::Literal(
                                Literal::Number(4.0),
                                Span::new(Rc::clone(&source), 5, 6),
                            )),
                        },),
                        Span::new(Rc::clone(&source), 0, 6),
                    ),)
                }),
                Span::new(Rc::clone(&source), 0, 6)
            ))
        )
    }

    #[test]
    fn test_invaild_expr() {
        let sources = vec![
            Source::source("12 + error"),
            Source::source("12 - error"),
            Source::source("12 * error"),
            Source::source("12 / error"),
            Source::source("12 < error"),
            Source::source("12 > error"),
            Source::source("12 <= error"),
            Source::source("12 >= error"),
            Source::source("12 == error"),
            Source::source("12 + 3 + error"),
            Source::source("12 + 3 - error"),
            Source::source("12 + 3 * error"),
            Source::source("12 + 3 / error"),
        ];

        for source in sources {
            let result = Parser::new(Rc::clone(&source)).parse();
            let err = match result {
                Err(err) => err,
                Ok(_) => unreachable!(),
            };
            assert_eq!(
                err.error,
                ExpectedError(ExpectedExpression(
                    "found unexpected token 'error'.".to_string()
                ))
            );
        }
    }

    #[test]
    fn test_unexpected_eof() {
        let sources = vec![
            Source::source("12 +"),
            Source::source("12 -"),
            Source::source("12 *"),
            Source::source("12 /"),
            Source::source("12 + 3 +"),
            Source::source("12 + 3 -"),
            Source::source("12 + 3 *"),
            Source::source("12 + 3 /"),
        ];

        for source in sources {
            let result = &Parser::new(Rc::clone(&source)).parse();
            let err = match result {
                Err(err) => err,
                Ok(_) => unreachable!(),
            };
            println!("failed on: {}", source.contents);
            assert_eq!(
                err.error,
                ExpectedError(ExpectedExpression("found unexpected '<Eof>'.".to_string()))
            );
        }
    }

    #[test]
    fn test_unbalanced_parentheses() {
        let sources = vec![
            (
                Source::source("(1 + 2"),
                ExpectedRightParen("found '<Eof>'.".to_string()),
            ),
            (
                Source::source("1 + (2 + 3"),
                ExpectedRightParen("found '<Eof>'.".to_string()),
            ),
            (
                Source::source("(1 + 2 ?"),
                ExpectedRightParen("found unexpected token '?'.".to_string()),
            ),
            (
                Source::source("(1 + (1 + (1 + (1 + 2)))?"),
                ExpectedRightParen("found unexpected token '?'.".to_string()),
            ),
            (
                Source::source("(1 + ?)"),
                ExpectedExpression("found unexpected token '?'.".to_string()),
            ),
        ];

        for (source, error) in sources {
            let result = &Parser::new(Rc::clone(&source)).parse();
            let err = match result {
                Err(err) => err,
                Ok(_) => unreachable!(),
            };
            println!("failed on: {}", source.contents);
            assert_eq!(err.error, ExpectedError(error));
        }
    }

    #[test]
    fn test_make_error() {
        let mut parser = Parser::new(Source::source(""));

        // if the first error is Unexpected return the second error.
        let first = ParserError::new(SyntaxError::UnexpectedError(UnexpectedEOF), &Span::empty());
        let second = ExpectedError(ExpectedExpression(first.error.to_string()));

        let from = parser.make_error(first, second);

        assert_eq!(
            from,
            ParserError::new(
                ExpectedError(ExpectedExpression(
                    SyntaxError::UnexpectedError(UnexpectedEOF).to_string(),
                )),
                &Span::empty()
            )
        );

        // If the first is expected, return the the first error.
        let first = ParserError::new(
            ExpectedError(ExpectedExpression("found error".to_string())),
            &Span::empty(),
        );
        let second = SyntaxError::UnexpectedError(UnexpectedEOF);

        let from = parser.make_error(first, second);

        assert_eq!(
            from,
            ParserError::new(
                ExpectedError(ExpectedExpression("found error".to_string())),
                &Span::empty(),
            )
        );

        // if the both are expected, return the first error.
        let first = ParserError::new(
            ExpectedError(ExpectedExpression("found error".to_string())),
            &Span::empty(),
        );
        let second = ExpectedError(ExpectedExpression("found error".to_string()));
        let from = parser.make_error(first, second);

        assert_eq!(
            from,
            ParserError::new(
                ExpectedError(ExpectedExpression("found error".to_string())),
                &Span::empty(),
            )
        )
    }
}
