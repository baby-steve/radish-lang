use std::mem;
use std::rc::Rc;

use crate::compiler::{
    ast::*,
    error::{SyntaxError, SyntaxErrorKind},
    scanner::Scanner,
    token::{Token, TokenType},
};

use crate::common::{Source, Span};
use crate::error::Item;

use super::{
    ast_ast::{
        AssignmentExpr, BinaryExpr, BinaryOp, BlockStmt, ClassDecl, ExpressionStmt, FunctionDecl,
        Identifier, LogicalExpr, LogicalOp, MethodDecl, OpAssignment, Position, Property, UnaryOp,
        VariableDecl, VariableKind,
    },
    pipeline::PipelineSettings,
};

struct ParserSettings {
    pub dump_ast: bool,
}

impl ParserSettings {
    pub fn new() -> Self {
        Self { dump_ast: false }
    }
}

impl Default for ParserSettings {
    fn default() -> Self {
        Self::new()
    }
}

impl From<&PipelineSettings> for ParserSettings {
    fn from(pipeline: &PipelineSettings) -> Self {
        Self {
            dump_ast: pipeline.dump_ast,
        }
    }
}

pub struct Parser {
    settings: ParserSettings,
    // source: Rc<Source>,
    scanner: Scanner,
    previous: Token,
    current: Token,
}

impl Parser {
    pub fn new(source: Rc<Source>, settings: &PipelineSettings) -> Self {
        Self {
            settings: ParserSettings::from(settings),
            // source: Rc::clone(&source),
            scanner: Scanner::new(source),
            previous: Token::empty(),
            current: Token::empty(),
        }
    }

    pub fn parse(&mut self) -> Result<AST, SyntaxError> {
        self.advance();

        match self.parse_body() {
            Ok(items) => {
                if self.settings.dump_ast {
                    println!("{:#?}", &items);
                }
                Ok(AST::new(items))
            }
            Err(err) => Err(err),
        }
    }

    fn advance(&mut self) {
        self.previous = mem::replace(&mut self.current, self.scanner.scan_token());
    }

    fn check(&self, token_type: &TokenType) -> bool {
        // self.scanner.scan_token().token_type == *token_type
        self.current.token_type == *token_type
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
                "Unexpected token '{}', expected '{:?}' at {:?}.",
                self.current.syntax(),
                token_type,
                self.current.span,
            );
        }
    }

    fn error(&mut self, err_kind: SyntaxErrorKind) -> SyntaxError {
        SyntaxError::new(err_kind)
    }

    fn expect(&mut self, expected: TokenType) -> Result<(), SyntaxError> {
        if !self.match_token(&expected) {
            let actual = self.current.clone();

            let err_kind = SyntaxErrorKind::Expected {
                expected: Item::new(&Span::empty(), expected.syntax()),
                actual: Item::new(&actual.span, actual.syntax()),
            };

            let err = SyntaxError::new(err_kind);

            Err(err)
        } else {
            Ok(())
        }
    }

    fn parse_body(&mut self) -> Result<Vec<Stmt>, SyntaxError> {
        let mut items = vec![];

        loop {
            match self.current.token_type {
                // <Eof>
                TokenType::Eof => {
                    self.advance();
                    return Ok(items);
                }
                // \n or //...
                TokenType::Newline | TokenType::Comment(_, false) => {
                    // should single-line comments be parsed?
                    self.advance();
                    continue;
                }
                // ...
                _ => items.push(self.parse_statement()?),
            }
        }
    }

    /// Parse everything up to, but not including, a delimiter.
    fn parse_block(&mut self) -> Result<BlockStmt, SyntaxError> {
        let opening_delimiter = self.previous.clone();

        let mut body = vec![];

        while !self.current.is_delimiter() {
            match self.current.token_type {
                // \n or //...
                TokenType::Newline | TokenType::Comment(_, false) => {
                    // should single-line comments be parsed?
                    self.advance();
                    continue;
                }
                // ...
                _ => body.push(self.parse_statement()?),
            };
        }

        let closing_delimiter = self.current.clone();

        match (&opening_delimiter.token_type, &closing_delimiter.token_type) {
            (TokenType::LeftBrace, TokenType::RightBrace)
            | (TokenType::Then, TokenType::EndIf)
            | (TokenType::Then, TokenType::Else)
            | (TokenType::Else, TokenType::EndIf)
            | (TokenType::Loop, TokenType::EndLoop)
            | (TokenType::LeftParen, TokenType::RightParen) => {
                let span = Span::combine(&opening_delimiter.span, &closing_delimiter.span);
                let block_stmt = BlockStmt { body, span };

                Ok(block_stmt)
            }
            _ => {
                let err_kind = SyntaxErrorKind::MismatchedDelimiter {
                    first: Item::new(&opening_delimiter.span, opening_delimiter.syntax()),
                    second: Item::new(&closing_delimiter.span, closing_delimiter.syntax()),
                };

                let err = self.error(err_kind);

                Err(err)
            }
        }
    }

    fn parse_statement(&mut self) -> Result<Stmt, SyntaxError> {
        match self.current.token_type {
            // "{" ...
            TokenType::LeftBrace => {
                self.consume(TokenType::LeftBrace);

                let block = self.parse_block()?;
                self.expect(TokenType::RightBrace)?;

                Ok(Stmt::BlockStmt(block))
            }
            // fun
            TokenType::Fun => Ok(Stmt::FunctionDecl(self.parse_fun_declaration(false)?)),
            // class
            TokenType::Class => self.parse_class_declaration(),
            // var
            TokenType::Var => Ok(Stmt::VariableDecl(self.parse_var_declaration(false)?)),
            // fin
            TokenType::Fin => Ok(Stmt::VariableDecl(self.parse_var_declaration(true)?)),
            // if
            TokenType::If => self.parse_if_statement(),
            // loop
            TokenType::Loop => self.parse_loop_statement(),
            // while
            TokenType::While => self.parse_while_statement(),
            // import
            TokenType::Import => self.parse_import_statement(),
            // return
            TokenType::Return => self.parse_return_statement(),
            // break
            TokenType::Break => self.parse_break_statement(),
            // continue
            TokenType::Continue => self.parse_continue_statement(),
            // print
            TokenType::Print => self.parse_print_statement(),
            // expr
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_fun_declaration(&mut self, constructor: bool) -> Result<FunctionDecl, SyntaxError> {
        let start = self.current.span.clone();

        if constructor {
            self.consume(TokenType::Con);
        } else {
            self.consume(TokenType::Fun);
        }

        let id = self.parse_identifier()?;

        let params = self.parse_params()?;

        self.expect(TokenType::LeftBrace)?;
        let body = self.parse_block()?;
        self.expect(TokenType::RightBrace)?;

        let span = Span::combine(&start, &self.current.span);

        let function = FunctionDecl {
            id,
            params,
            body,
            span,
            scope: None,
        };

        Ok(function)
    }

    fn parse_class_declaration(&mut self) -> Result<Stmt, SyntaxError> {
        let start = self.current.span.clone();

        // consume the class keyword
        self.consume(TokenType::Class);

        // parse the class's name.
        let id = self.parse_identifier()?;

        self.expect(TokenType::LeftBrace)?;

        let mut body = vec![];
        while self.current.token_type != TokenType::RightBrace {
            // TODO: skip newlines.
            self.match_token(&TokenType::Newline);

            match &self.current.token_type {
                TokenType::Var => {
                    let variable_decl = self.parse_var_declaration(false)?;

                    body.push(Stmt::VariableDecl(variable_decl));
                }
                TokenType::Fin => {
                    let variable_decl = self.parse_var_declaration(true)?;

                    body.push(Stmt::VariableDecl(variable_decl));
                }
                TokenType::Fun => {
                    let method = self.parse_fun_declaration(false)?;

                    let method_decl = MethodDecl::method(method);

                    body.push(Stmt::MethodDecl(method_decl));
                }
                TokenType::Con => {
                    let constructor = self.parse_fun_declaration(true)?;

                    let constructor_decl = MethodDecl::constructor(constructor);

                    body.push(Stmt::MethodDecl(constructor_decl));
                }
                err => {
                    let err = SyntaxError::new(SyntaxErrorKind::Unexpected {
                        found: Item::new(&self.current.span, err.syntax()),
                    });
                    return Err(err);
                }
            }
        }

        self.expect(TokenType::RightBrace)?;

        // get the span for the whole class.
        let span = Span::combine(&start, &self.current.span);

        let class = ClassDecl { id, body, span };
        Ok(Stmt::ClassDecl(class))
    }

    fn parse_var_declaration(&mut self, constant: bool) -> Result<VariableDecl, SyntaxError> {
        // var|fin ...
        let start = Span::from(&self.current.span);

        if constant {
            self.consume(TokenType::Fin);
        } else {
            self.consume(TokenType::Var);
        }

        // var|fin id ...
        let id = self.parse_identifier()?;

        let current = &self.current;

        let (init, span) = match current.token_type {
            TokenType::Equals => {
                self.consume(TokenType::Equals);
                let init = self.parse_expression()?;
                let span = Span::combine(&start, &init.position());
                (Some(init), span)
            }
            _ => (None, Span::combine(&start, &current.span)),
        };

        let kind = if constant {
            VariableKind::Final
        } else {
            VariableKind::Variable
        };

        Ok(VariableDecl {
            id,
            init,
            kind,
            span,
        })
    }

    fn parse_if_statement(&mut self) -> Result<Stmt, SyntaxError> {
        // if ...
        let start = Span::from(&self.current.span);
        self.consume(TokenType::If);

        // if <expr> ...
        let expr = self.parse_expression()?;

        // if <expr> then ...
        self.expect(TokenType::Then)?;

        // if <expr> then <block> ...
        let block = self.parse_block()?;

        let alt = if self.match_token(&TokenType::Else) {
            // if <expr> then <block> else ...
            if self.check(&TokenType::If) {
                // if <expr> then <block> else if ...
                Some(Box::new(self.parse_if_statement()?))
            } else {
                // if <expr> then <block> else <block> endif
                let alternate = self.parse_block()?;
                self.expect(TokenType::EndIf)?;
                Some(Box::new(Stmt::BlockStmt(alternate)))
            }
        } else {
            // if <expr> then <block> endif
            self.expect(TokenType::EndIf)?;
            None
        };

        Ok(AST::if_stmt(
            expr,
            block,
            alt,
            Span::combine(&start, &self.current.span),
        ))
    }

    fn parse_loop_statement(&mut self) -> Result<Stmt, SyntaxError> {
        let start = self.current.span.clone();

        // loop ...
        self.consume(TokenType::Loop);

        // loop <body> ...
        let loop_body = self.parse_block()?;

        // loop <body> endloop
        self.expect(TokenType::EndLoop)?;

        Ok(AST::loop_stmt(
            loop_body,
            Span::combine(&start, &self.current.span),
        ))
    }

    fn parse_while_statement(&mut self) -> Result<Stmt, SyntaxError> {
        let start = self.current.span.clone();

        // while ...
        self.consume(TokenType::While);

        // while <expr> ...
        let condition = self.parse_expression()?;

        // while <expr> loop ...
        self.expect(TokenType::Loop)?;

        // while <expr> loop <body> ...
        let loop_body = self.parse_block()?;

        // while <expr> loop <body> endloop
        self.expect(TokenType::EndLoop)?;

        Ok(AST::while_stmt(
            condition,
            loop_body,
            Span::combine(&start, &self.current.span),
        ))
    }

    fn parse_import_statement(&mut self) -> Result<Stmt, SyntaxError> {
        let start = self.current.span.clone();

        self.consume(TokenType::Import);

        let path = match self.current.token_type.clone() {
            TokenType::String(val) => {
                self.advance();
                val.to_string()
            }
            // FIXME: definitely should handle this better.
            _ => panic!("path must be a string literal"),
        };

        let items = if self.match_token(&TokenType::For) {
            let mut items = vec![self.parse_identifier()?];

            while self.match_token(&TokenType::Comma) {
                items.push(self.parse_identifier()?);
            }

            items
        } else {
            vec![]
        };

        let span = Span::combine(&start, &self.current.span);

        Ok(AST::import_stmt(path, items, span))
    }

    fn parse_break_statement(&mut self) -> Result<Stmt, SyntaxError> {
        // Todo: should be able to break to a label.
        self.consume(TokenType::Break);

        Ok(AST::break_stmt(Span::from(&self.previous.span)))
    }

    fn parse_continue_statement(&mut self) -> Result<Stmt, SyntaxError> {
        // Todo: should be able to continue to a label.
        self.consume(TokenType::Continue);

        Ok(AST::continue_stmt(Span::from(&self.previous.span)))
    }

    fn parse_return_statement(&mut self) -> Result<Stmt, SyntaxError> {
        let start = self.current.span.clone();

        // return ...
        self.consume(TokenType::Return);

        let (return_val, span) = match self.current.token_type {
            // return '\n'
            TokenType::Newline => (None, start),
            // return <expr>
            _ => {
                let val = self.parse_expression()?;
                let span = Span::combine(&start, &val.position());
                (Some(val), span)
            }
        };

        Ok(AST::return_stmt(return_val, span))
    }

    fn parse_print_statement(&mut self) -> Result<Stmt, SyntaxError> {
        let start = self.current.clone().span;

        self.consume(TokenType::Print);

        // "print" <expr>
        let expr = self.parse_expression()?;
        let end = expr.position();

        let span = Span::combine(&start, &end);

        Ok(AST::print_stmt(expr, span))
    }

    fn parse_expression_statement(&mut self) -> Result<Stmt, SyntaxError> {
        let expression_stmt = ExpressionStmt::new(self.parse_expression()?);

        Ok(Stmt::ExpressionStmt(expression_stmt))
    }

    fn parse_assignment_expression(&mut self) -> Result<Expr, SyntaxError> {
        let lhs = self.parse_boolean_expression()?;

        let op = match self.current.token_type {
            // expr = ...
            TokenType::Equals => {
                self.consume(TokenType::Equals);
                OpAssignment::Equals
            }
            // expr += ...
            TokenType::PlusEquals => {
                self.consume(TokenType::PlusEquals);
                OpAssignment::AddAssign
            }
            // expr -= ...
            TokenType::MinusEquals => {
                self.consume(TokenType::MinusEquals);
                OpAssignment::SubAssign
            }
            // expr *= ...
            TokenType::MultiplyEquals => {
                self.consume(TokenType::MultiplyEquals);
                OpAssignment::MulAssign
            }
            // expr /= ...
            TokenType::DivideEquals => {
                self.consume(TokenType::DivideEquals);
                OpAssignment::DivAssign
            }
            // expr %= ...
            TokenType::ModuloEquals => {
                self.consume(TokenType::ModuloEquals);
                OpAssignment::RemAssign
            }
            // expr
            _ => return Ok(lhs),
        };

        // id op ....
        let rhs = self.parse_boolean_expression()?;

        let span = Span::combine(&lhs.position(), &rhs.position());

        let assignment_expr = AssignmentExpr {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
            span,
        };

        Ok(Expr::AssignmentExpr(assignment_expr))
    }

    fn parse_expression(&mut self) -> Result<Expr, SyntaxError> {
        match self.parse_assignment_expression() {
            Ok(expr) => Ok(expr),
            Err(err) => match err.kind {
                SyntaxErrorKind::Unexpected { found } => {
                    let err_kind = SyntaxErrorKind::ExpectedExpression { actual: found };
                    let err = self.error(err_kind);
                    Err(err)
                }
                SyntaxErrorKind::UnexpectedEof { ref location } => {
                    let err_kind = SyntaxErrorKind::ExpectedExpression {
                        actual: Item::new(location, "<eof>"),
                    };
                    let expected_expr = self.error(err_kind).set_cause(err);
                    Err(expected_expr)
                }
                _ => Err(err),
            },
        }
    }

    fn parse_boolean_expression(&mut self) -> Result<Expr, SyntaxError> {
        let mut node = self.parse_boolean_term()?;

        while let TokenType::Or = self.current.token_type {
            self.consume(TokenType::Or);

            let right = self.parse_boolean_term()?;
            let span = Span::combine(&node.position(), &right.position());

            node = Expr::LogicalExpr(LogicalExpr {
                lhs: Box::new(node),
                op: LogicalOp::Or,
                rhs: Box::new(right),
                span,
            })
        }

        Ok(node)
    }

    fn parse_boolean_term(&mut self) -> Result<Expr, SyntaxError> {
        let mut node = self.parse_boolean_factor()?;

        while let TokenType::And = self.current.token_type {
            self.consume(TokenType::And);

            let right = self.parse_boolean_factor()?;
            let span = Span::combine(&node.position(), &right.position());

            node = Expr::LogicalExpr(LogicalExpr {
                lhs: Box::new(node),
                op: LogicalOp::And,
                rhs: Box::new(right),
                span,
            })
        }

        Ok(node)
    }

    fn parse_boolean_factor(&mut self) -> Result<Expr, SyntaxError> {
        let mut node = self.parse_sum()?;

        loop {
            match self.current.token_type {
                // expr < ...
                TokenType::LessThan => {
                    self.consume(TokenType::LessThan);

                    let right = self.parse_sum()?;

                    let span = Span::combine(&node.position(), &right.position());
                    node = AST::binary_expr(BinaryExpr::new(BinaryOp::LessThan, node, right, span))
                }
                // expr <= ...
                TokenType::LessThanEquals => {
                    self.consume(TokenType::LessThanEquals);

                    let right = self.parse_sum()?;

                    let span = Span::combine(&node.position(), &right.position());
                    node = AST::binary_expr(BinaryExpr::new(
                        BinaryOp::LessThanEquals,
                        node,
                        right,
                        span,
                    ))
                }
                // expr > ...
                TokenType::GreaterThan => {
                    self.consume(TokenType::GreaterThan);

                    let right = self.parse_sum()?;

                    let span = Span::combine(&node.position(), &right.position());
                    node =
                        AST::binary_expr(BinaryExpr::new(BinaryOp::GreaterThan, node, right, span))
                }
                // expr >= ...
                TokenType::GreaterThanEquals => {
                    self.consume(TokenType::GreaterThanEquals);

                    let right = self.parse_sum()?;

                    let span = Span::combine(&node.position(), &right.position());
                    node = AST::binary_expr(BinaryExpr::new(
                        BinaryOp::GreaterThanEquals,
                        node,
                        right,
                        span,
                    ))
                }
                // expr == ...
                TokenType::EqualsTo => {
                    self.consume(TokenType::EqualsTo);

                    let right = self.parse_sum()?;
                    let span = Span::combine(&node.position(), &right.position());
                    node = AST::binary_expr(BinaryExpr::new(BinaryOp::EqualsTo, node, right, span))
                }
                // expr != ...
                TokenType::NotEqual => {
                    self.consume(TokenType::NotEqual);

                    let right = self.parse_sum()?;

                    let span = Span::combine(&node.position(), &right.position());
                    node = AST::binary_expr(BinaryExpr::new(BinaryOp::NotEqual, node, right, span))
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn parse_sum(&mut self) -> Result<Expr, SyntaxError> {
        // expr ...
        let mut node = self.parse_term()?;

        loop {
            match self.current.token_type {
                // expr + ...
                TokenType::Plus => {
                    self.consume(TokenType::Plus);

                    let right = self.parse_term()?;

                    let span = Span::combine(&node.position(), &right.position());
                    node = AST::binary_expr(BinaryExpr::new(BinaryOp::Add, node, right, span))
                }
                // expr - ...
                TokenType::Minus => {
                    self.consume(TokenType::Minus);

                    let right = self.parse_term()?;

                    let span = Span::combine(&node.position(), &right.position());

                    node = AST::binary_expr(BinaryExpr::new(BinaryOp::Subtract, node, right, span))
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn parse_term(&mut self) -> Result<Expr, SyntaxError> {
        // expr ...
        let mut node = self.parse_member()?;

        loop {
            match self.current.token_type {
                // expr * ...
                TokenType::Star => {
                    self.consume(TokenType::Star);

                    let right = self.parse_member()?;

                    let span = Span::combine(&node.position(), &right.position());
                    node = AST::binary_expr(BinaryExpr::new(BinaryOp::Multiply, node, right, span))
                }
                // expr / ...
                TokenType::Slash => {
                    self.consume(TokenType::Slash);

                    let right = self.parse_member()?;

                    let span = Span::combine(&node.position(), &right.position());
                    node = AST::binary_expr(BinaryExpr::new(BinaryOp::Divide, node, right, span))
                }
                // expr % ...
                TokenType::Percent => {
                    self.consume(TokenType::Percent);

                    let right = self.parse_member()?;

                    let span = Span::combine(&node.position(), &right.position());
                    node = AST::binary_expr(BinaryExpr::new(BinaryOp::Remainder, node, right, span))
                }
                _ => break,
            };
        }

        Ok(node)
    }

    fn parse_member(&mut self) -> Result<Expr, SyntaxError> {
        let mut node = self.parse_factor()?;

        loop {
            match &self.current.token_type {
                // <expr> '(' ...
                TokenType::LeftParen => {
                    let args = self.parse_arg_list()?;
                    let span = Span::combine(&node.position(), &self.current.span);
                    node = AST::call_expr(Box::new(node), args, span)
                }
                // <expr> '.' ...
                TokenType::Dot => {
                    self.consume(TokenType::Dot);
                    let property = Box::new(self.parse_factor()?);
                    let span = Span::combine(&node.position(), &self.current.span);
                    node = AST::member_expr(Box::new(node), property, false, span)
                }
                // <expr> '[' ...
                TokenType::LeftBracket => {
                    self.consume(TokenType::LeftBracket);
                    let property = Box::new(self.parse_sum()?);
                    self.consume(TokenType::RightBracket);
                    let span = Span::combine(&node.position(), &self.current.span);
                    node = AST::member_expr(Box::new(node), property, true, span)
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn parse_factor(&mut self) -> Result<Expr, SyntaxError> {
        loop {
            let current = self.current.clone();

            match current.token_type {
                // <number>
                TokenType::Number(val) => {
                    let span = Span::from(&current.span);
                    let node = AST::number(val, span);
                    self.consume(TokenType::Number(val));
                    return Ok(node);
                }
                // <string>
                TokenType::String(val) => {
                    let span = Span::from(&current.span);
                    let node = AST::string(val.to_string(), span);
                    self.consume(TokenType::String(val));
                    return Ok(node);
                }
                // ( ...
                TokenType::LeftParen => return self.parse_paren(),
                // [
                TokenType::LeftBracket => return self.parse_array_literal(),
                // {
                TokenType::LeftBrace => return self.parse_map_literal(),
                // - ...
                TokenType::Minus => {
                    self.consume(TokenType::Minus);
                    let arg = self.parse_factor()?;
                    let span = Span::combine(&current.span, &arg.position());

                    let node = AST::unary_expr(UnaryOp::Minus, Box::new(arg), span);
                    return Ok(node);
                }
                // ! ...
                TokenType::Bang => {
                    self.consume(TokenType::Bang);

                    let arg = self.parse_factor()?;
                    let span = Span::combine(&current.span, &arg.position());

                    let node = AST::unary_expr(UnaryOp::Not, Box::new(arg), span);
                    return Ok(node);
                }
                // "true"
                TokenType::True => {
                    let span = Span::from(&current.span);
                    let node = AST::bool(true, span);
                    self.consume(TokenType::True);
                    return Ok(node);
                }
                // "false"
                TokenType::False => {
                    let span = Span::from(&current.span);
                    let node = AST::bool(false, span);
                    self.consume(TokenType::False);
                    return Ok(node);
                }
                // "nil"
                TokenType::Nil => {
                    let span = Span::from(&current.span);
                    let node = AST::nil(span);
                    self.consume(TokenType::Nil);
                    return Ok(node);
                }
                // <id>
                TokenType::Ident(name) => {
                    let id = Identifier::new(&name, Span::from(&current.span));

                    let node = AST::identifier(id);

                    self.consume(TokenType::Ident(name));
                    return Ok(node);
                }
                // this
                TokenType::This => {
                    let span = Span::from(&current.span);
                    let node = AST::this(span);
                    self.consume(TokenType::This);
                    return Ok(node);
                }
                // <eof>
                TokenType::Eof => {
                    // if an <eof> token is found here, it has to be an error.
                    return Err(self.error(SyntaxErrorKind::UnexpectedEof {
                        location: self.current.span.clone(),
                    }));
                }
                // single-line comment
                TokenType::Comment(_, false) => {
                    self.advance();
                    continue;
                }
                // \n
                TokenType::Newline => {
                    self.advance();
                    continue;
                }
                // lexer error
                TokenType::Error(err) => {
                    let message = err.to_string();
                    let err = self.error(SyntaxErrorKind::Unexpected {
                        found: Item::new(&current.span, message),
                    });
                    return Err(err);
                }
                _ => {
                    let current = self.current.clone();
                    let err_kind = SyntaxErrorKind::Unexpected {
                        found: Item::new(&current.span, current.syntax()),
                    };
                    let err = self.error(err_kind);
                    return Err(err);
                }
            }
        }
    }

    fn parse_identifier(&mut self) -> Result<Identifier, SyntaxError> {
        let token = self.current.clone();

        match token.token_type {
            // id
            TokenType::Ident(id) => {
                self.consume(TokenType::Ident(id.clone()));

                Ok(Identifier::new(id, Span::from(&token.span)))
            }
            // <error>
            _ => {
                let err_kind = SyntaxErrorKind::ExpectedIdent {
                    actual: Item::new(&self.current.span, token.syntax()),
                };
                let err = SyntaxError::new(err_kind);
                Err(err)
            }
        }
    }

    fn parse_arg_list(&mut self) -> Result<Vec<Expr>, SyntaxError> {
        self.consume(TokenType::LeftParen);

        let mut args = vec![];

        if !self.check(&TokenType::RightParen) {
            loop {
                args.push(self.parse_expression()?);
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }

        self.expect(TokenType::RightParen)?;

        Ok(args)
    }

    fn parse_params(&mut self) -> Result<Vec<Identifier>, SyntaxError> {
        self.expect(TokenType::LeftParen)?;

        let mut args = vec![];

        if !self.check(&TokenType::RightParen) {
            loop {
                args.push(self.parse_identifier()?);
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }

        self.expect(TokenType::RightParen)?;

        Ok(args)
    }

    fn parse_paren(&mut self) -> Result<Expr, SyntaxError> {
        // ( ...
        self.consume(TokenType::LeftParen);

        let expr = self.parse_sum()?;

        // ( expr )
        self.expect(TokenType::RightParen)?;

        Ok(expr)
    }

    fn parse_array_literal(&mut self) -> Result<Expr, SyntaxError> {
        // TODO: trailing comma?

        let start = self.current.span.clone();
        let mut elements = vec![];

        // [ ...
        self.consume(TokenType::LeftBracket);

        // [ x, y, z ...
        if !self.check(&TokenType::RightBracket) {
            loop {
                let expr = self.parse_sum()?;
                elements.push(expr);

                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }

        // [ ... ]
        self.expect(TokenType::RightBracket)?;

        let span = Span::combine(&start, &self.current.span);

        Ok(AST::array(elements, span))
    }

    fn parse_map_literal(&mut self) -> Result<Expr, SyntaxError> {
        // TODO: trailing comma?

        let start = self.current.span.clone();
        let mut properties = vec![];

        // { ...
        self.consume(TokenType::LeftBrace);

        // { x: a, y: b, z: c ...
        if !self.check(&TokenType::RightBrace) {
            loop {
                let key = self.parse_sum()?;
                self.expect(TokenType::Colon)?;
                let value = self.parse_sum()?;
                let span = Span::empty();

                let prop = Property { key, value, span };

                properties.push(prop);

                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }

        // { ... }
        self.expect(TokenType::RightBrace)?;

        let span = Span::combine(&start, &self.current.span);

        Ok(AST::map(properties, span))
    }
}
