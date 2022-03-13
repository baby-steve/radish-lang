use std::mem;
use std::rc::Rc;

use crate::compiler::{
    ast::*,
    error::{SyntaxError, SyntaxErrorKind},
    scanner::Scanner,
    token::{Token, TokenType},
};

use crate::RadishConfig;

use crate::common::{source::Source, span::Span};
use crate::error::Item;

pub struct Parser {
    config: Rc<RadishConfig>,
    source: Rc<Source>,
    scanner: Scanner,
    previous: Token,
    current: Token,
}

impl Parser {
    pub fn new(source: Rc<Source>, config: &Rc<RadishConfig>) -> Parser {
        Parser {
            config: Rc::clone(&config),
            source: Rc::clone(&source),
            scanner: Scanner::new(source),
            previous: Token::empty(),
            current: Token::empty(),
        }
    }

    pub fn parse(&mut self) -> Result<AST, SyntaxError> {
        self.advance();

        match self.parse_body() {
            Ok(items) => { 
                if self.config.dump_ast {
                    println!("{:#?}", &items);
                }
                Ok(AST::new(items)) 
            },
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
    fn parse_block(&mut self) -> Result<Vec<Stmt>, SyntaxError> {
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
            | (TokenType::LeftParen, TokenType::RightParen) => return Ok(body),
            _ => {
                let err_kind = SyntaxErrorKind::MismatchedDelimiter {
                    first: Item::new(&opening_delimiter.span, opening_delimiter.syntax()),
                    second: Item::new(&closing_delimiter.span, closing_delimiter.syntax()),
                };

                let err = self.error(err_kind);

                return Err(err);
            }
        }
    }

    fn parse_statement(&mut self) -> Result<Stmt, SyntaxError> {
        match self.current.token_type {
            // "{" ...
            TokenType::LeftBrace => {
                let start = self.current.span.clone();
                self.consume(TokenType::LeftBrace);

                let block = self.parse_block()?;
                self.expect(TokenType::RightBrace)?;

                Ok(AST::block_stmt(
                    block,
                    Span::combine(&start, &self.current.span),
                ))
            }
            // fun
            TokenType::Fun => self.parse_fun_declaration(),
            // class
            TokenType::Class => self.parse_class_declaration(),
            // var
            TokenType::Var => self.parse_var_declaration(false),
            // fin
            TokenType::Fin => self.parse_var_declaration(true),
            // if
            TokenType::If => self.parse_if_statement(),
            // loop
            TokenType::Loop => self.parse_loop_statement(),
            // while
            TokenType::While => self.parse_while_statement(),
            // return
            TokenType::Return => self.parse_return_statement(),
            // break
            TokenType::Break => self.parse_break_statement(),
            // continue
            TokenType::Continue => self.parse_continue_statement(),
            // print
            TokenType::Print => self.parse_print_statement(),
            // id
            TokenType::Ident(_) => self.parse_assignment_statement(),
            // expr
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_fun_declaration(&mut self) -> Result<Stmt, SyntaxError> {
        let start = self.current.span.clone();

        // fun ...
        self.consume(TokenType::Fun);

        // fun id ...
        let id = self.parse_identifier()?;

        // fun id '(' <params> ')' ...
        let params = self.parse_params()?;

        // fun id '(' <params> ')' '{' ...
        self.expect(TokenType::LeftBrace)?;

        // fun id '(' <params> ')' '{' <body> ...
        let body = self.parse_block()?;

        // fun id '(' <params> ')' '{' <body> '}'
        self.expect(TokenType::RightBrace)?;

        let span = Span::combine(&start, &self.current.span);

        let function = FunctionDecl::new(id, params, body);

        Ok(AST::fun_decl(function, span))
    }

    fn parse_class_declaration(&mut self) -> Result<Stmt, SyntaxError> {    
        let start = self.current.span.clone();
        let mut constructors = vec![];

        // class ...
        self.consume(TokenType::Class);

        // class <id> ...
        let id = self.parse_identifier()?;

        // class <id> { ...
        self.expect(TokenType::LeftBrace)?;

        // parse class body
        while self.current.token_type != TokenType::RightBrace {
            match self.current.token_type {
                // \n or //...
                TokenType::Newline | TokenType::Comment(_, false) => {
                    // should single-line comments be parsed?
                    self.advance();
                    continue;
                }
                // a constructor
                TokenType::Con => constructors.push(self.parse_constructor()?),
                _ => {
                    panic!("parsing a class");
                    // break;
                }
            };
        };

        // class <id> { ... }
        self.expect(TokenType::RightBrace)?;

        let class = ClassDecl::new(id, constructors);
        let span = Span::combine(&start, &self.current.span);
        Ok(AST::class_decl(class, span))
    }

    fn parse_constructor(&mut self) -> Result<ConstructorDecl, SyntaxError> {
        // con ... 
        self.consume(TokenType::Con);
        // con <id> ...
        let id = self.parse_identifier()?;
        // con <id> '(' <params> ')' ...
        let params = self.parse_params()?;
        // con <id> '(' <params> ')' '{' ...
        self.expect(TokenType::LeftBrace)?;
        // con <id> '(' <params> ')' '{' <body> ...
        let body = Box::new(self.parse_block()?);
        // con <id> '(' <params> ')' '{' <body> '}'
        self.expect(TokenType::RightBrace)?;

        let constructor = ConstructorDecl::new(id, params, body);

        Ok(constructor)
    }

    fn parse_var_declaration(&mut self, constant: bool) -> Result<Stmt, SyntaxError> {
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

        let var_kind = if constant { VarKind::Fin } else { VarKind::Var };

        Ok(AST::var_decl(id, init, var_kind, span))
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
                Some(Box::new(AST::block_stmt(
                    alternate,
                    Span::combine(&start, &self.current.span),
                )))
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
        let end = &expr.position();

        Ok(AST::print_stmt(expr, Span::combine(&start, end)))
    }

    fn parse_expression_statement(&mut self) -> Result<Stmt, SyntaxError> {
        Ok(AST::expr_stmt(Box::new(self.parse_expression()?)))
    }

    fn parse_assignment_statement(&mut self) -> Result<Stmt, SyntaxError> {
        let node = self.parse_expression()?;

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
            _ => return Ok(Stmt::ExpressionStmt(Box::new(node))),
        };

        let id = match node {
            Expr::Identifier(id) => id,
            _ => {
                let err_kind = SyntaxErrorKind::ExpectedIdent {
                    actual: Item::new(&self.current.span, self.current.syntax()),
                };
                let err = SyntaxError::new(err_kind);
                return Err(err);
            }
        };

        // id op ....
        let right = self.parse_expression()?;

        let span = Span::combine(&id.pos, &right.position());
        Ok(AST::assignment(id, op, right, span))
    }

    fn parse_expression(&mut self) -> Result<Expr, SyntaxError> {
        match self.parse_boolean_expression() {
            Ok(expr) => Ok(expr),
            Err(err) => match err.kind {
                SyntaxErrorKind::Unexpected { found } => {
                    let err_kind = SyntaxErrorKind::ExpectedExpression { actual: found };
                    let err = self.error(err_kind);
                    return Err(err);
                }
                SyntaxErrorKind::UnexpectedEof { ref location } => {
                    let err_kind = SyntaxErrorKind::ExpectedExpression {
                        actual: Item::new(&location, "<eof>"),
                    };
                    let expected_expr = self.error(err_kind).set_cause(err);
                    return Err(expected_expr);
                }
                _ => return Err(err),
            },
        }
    }

    fn parse_boolean_expression(&mut self) -> Result<Expr, SyntaxError> {
        let mut node = self.parse_boolean_term()?;

        loop {
            match self.current.token_type {
                TokenType::Or => {
                    self.consume(TokenType::Or);

                    let right = self.parse_boolean_term()?;

                    let span = Span::combine(&node.position(), &right.position());
                    node = AST::logical_expr(Box::new(BinaryExpr::new(Op::Or, node, right)), span)
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn parse_boolean_term(&mut self) -> Result<Expr, SyntaxError> {
        let mut node = self.parse_boolean_factor()?;

        loop {
            match self.current.token_type {
                TokenType::And => {
                    self.consume(TokenType::And);

                    let right = self.parse_boolean_factor()?;

                    let span = Span::combine(&node.position(), &right.position());
                    node = AST::logical_expr(Box::new(BinaryExpr::new(Op::And, node, right)), span)
                }
                _ => break,
            }
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
                    node =
                        AST::binary_expr(Box::new(BinaryExpr::new(Op::LessThan, node, right)), span)
                }
                // expr <= ...
                TokenType::LessThanEquals => {
                    self.consume(TokenType::LessThanEquals);

                    let right = self.parse_sum()?;

                    let span = Span::combine(&node.position(), &right.position());
                    node = AST::binary_expr(
                        Box::new(BinaryExpr::new(Op::LessThanEquals, node, right)),
                        span,
                    )
                }
                // expr > ...
                TokenType::GreaterThan => {
                    self.consume(TokenType::GreaterThan);

                    let right = self.parse_sum()?;

                    let span = Span::combine(&node.position(), &right.position());
                    node = AST::binary_expr(
                        Box::new(BinaryExpr::new(Op::GreaterThan, node, right)),
                        span,
                    )
                }
                // expr >= ...
                TokenType::GreaterThanEquals => {
                    self.consume(TokenType::GreaterThanEquals);

                    let right = self.parse_sum()?;

                    let span = Span::combine(&node.position(), &right.position());
                    node = AST::binary_expr(
                        Box::new(BinaryExpr::new(Op::GreaterThanEquals, node, right)),
                        span,
                    )
                }
                // expr == ...
                TokenType::EqualsTo => {
                    self.consume(TokenType::EqualsTo);

                    let right = self.parse_sum()?;
                    let span = Span::combine(&node.position(), &right.position());
                    node =
                        AST::binary_expr(Box::new(BinaryExpr::new(Op::EqualsTo, node, right)), span)
                }
                // expr != ...
                TokenType::NotEqual => {
                    self.consume(TokenType::NotEqual);

                    let right = self.parse_sum()?;

                    let span = Span::combine(&node.position(), &right.position());
                    node =
                        AST::binary_expr(Box::new(BinaryExpr::new(Op::NotEqual, node, right)), span)
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
                    node = AST::binary_expr(Box::new(BinaryExpr::new(Op::Add, node, right)), span)
                }
                // expr - ...
                TokenType::Minus => {
                    self.consume(TokenType::Minus);

                    let right = self.parse_term()?;

                    let span = Span::combine(&node.position(), &right.position());

                    node =
                        AST::binary_expr(Box::new(BinaryExpr::new(Op::Subtract, node, right)), span)
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
                    node =
                        AST::binary_expr(Box::new(BinaryExpr::new(Op::Multiply, node, right)), span)
                }
                // expr / ...
                TokenType::Slash => {
                    self.consume(TokenType::Slash);

                    let right = self.parse_member()?;

                    let span = Span::combine(&node.position(), &right.position());
                    node =
                        AST::binary_expr(Box::new(BinaryExpr::new(Op::Divide, node, right)), span)
                }
                // expr % ...
                TokenType::Percent => {
                    self.consume(TokenType::Percent);

                    let right = self.parse_member()?;

                    let span = Span::combine(&node.position(), &right.position());
                    node = AST::binary_expr(
                        Box::new(BinaryExpr::new(Op::Remainder, node, right)),
                        span,
                    )
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
                    node = AST::member_expr(Box::new(node), property, span)
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
                // - ...
                TokenType::Minus => {
                    self.consume(TokenType::Minus);
                    let arg = self.parse_factor()?;
                    let span = Span::combine(&current.span, &arg.position());

                    let node = AST::unary_expr(Op::Subtract, Box::new(arg), span);
                    return Ok(node);
                }
                // ! ...
                TokenType::Bang => {
                    self.consume(TokenType::Bang);

                    let arg = self.parse_factor()?;
                    let span = Span::combine(&current.span, &arg.position());

                    let node = AST::unary_expr(Op::Bang, Box::new(arg), span);
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
                    let id = Ident {
                        name: name.to_string(),
                        pos: Span::from(&current.span),
                    };

                    let node = AST::identifier(id);
                    
                    self.consume(TokenType::Ident(name));
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
                    let err = self.error(SyntaxErrorKind::Custom { message });
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

    fn parse_identifier(&mut self) -> Result<Ident, SyntaxError> {
        let token = self.current.clone();

        match token.token_type {
            // id
            TokenType::Ident(id) => {
                self.consume(TokenType::Ident(id.clone()));

                Ok(Ident {
                    name: id.to_string(),
                    pos: Span::from(&token.span),
                })
            }
            // <error>
            _ => {
                let err_kind = SyntaxErrorKind::ExpectedIdent {
                    actual: Item::new(&self.current.span, token.syntax()),
                };
                let err = SyntaxError::new(err_kind);
                return Err(err);
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

    fn parse_params(&mut self) -> Result<Vec<Ident>, SyntaxError> {
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
        let start = self.current.span.clone();

        // ( ...
        self.consume(TokenType::LeftParen);

        let expr = self.parse_sum()?;

        // ( expr )
        self.expect(TokenType::RightParen)?;

        let span = Span::new(
            Rc::clone(&self.source),
            start.start,
            expr.position().end + 1,
        );

        Ok(AST::paren_expr(Box::new(expr), span))
    }
}
/*
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
    fn parse_nil_literal() {
        let source = Source::source("nil");
        let result = &Parser::new(Rc::clone(&source)).parse().unwrap().items[0];

        assert_eq!(
            *result,
            ASTNode::Stmt(Stmt::ExpressionStmt(
                Box::new(ExpressionStmt {
                    expr: ASTNode::Expr(Expr::Literal(
                        Literal::Nil,
                        Span::new(Rc::clone(&source), 0, 3)
                    ))
                }),
                Span::new(Rc::clone(&source), 0, 3)
            ))
        );
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
    fn parse_var_declaration() {
        let source = Source::source("var x");
        let result = &Parser::new(Rc::clone(&source)).parse().unwrap().items[0];

        assert_eq!(
            *result,
            ASTNode::from(Stmt::VarDeclaration(
                Box::new(VarDeclaration {
                    id: Ident {
                        name: "x".to_string(),
                        pos: Span::new(Rc::clone(&source), 4, 5)
                    },
                    init: None,
                }),
                Span::new(Rc::clone(&source), 0, 5)
            ))
        );
    }

    #[test]
    fn parse_var_declaration_with_value() {
        let source = Source::source("var x = 23");
        let result = &Parser::new(Rc::clone(&source)).parse().unwrap().items[0];

        assert_eq!(
            *result,
            ASTNode::from(Stmt::VarDeclaration(
                Box::new(VarDeclaration {
                    id: Ident {
                        name: "x".to_string(),
                        pos: Span::new(Rc::clone(&source), 4, 5)
                    },
                    init: Some(ASTNode::from(Expr::Literal(
                        Literal::Number(23.0),
                        Span::new(Rc::clone(&source), 8, 10)
                    )))
                }),
                Span::new(Rc::clone(&source), 0, 10)
            ))
        );
    }

    #[test]
    fn parse_print_statement() {
        let source = Source::source("print 23");
        let result = &Parser::new(Rc::clone(&source)).parse().unwrap().items[0];

        assert_eq!(
            *result,
            ASTNode::from(Stmt::PrintStmt(
                Box::new(ASTNode::from(Expr::Literal(
                    Literal::Number(23.0),
                    Span::new(Rc::clone(&source), 6, 8)
                ))),
                Span::new(Rc::clone(&source), 0, 8),
            ))
        );
    }

    #[test]
    fn parse_block_statement() {
        let source = Source::source("{ 23 }");
        let result = &Parser::new(Rc::clone(&source)).parse().unwrap().items[0];

        assert_eq!(
            *result,
            ASTNode::from(Stmt::BlockStmt(
                Box::new(BlockStmt {
                    body: vec![ASTNode::Stmt(Stmt::ExpressionStmt(
                        Box::new(ExpressionStmt {
                            expr: ASTNode::Expr(Expr::Literal(
                                Literal::Number(23.0),
                                Span::new(Rc::clone(&source), 2, 4)
                            ))
                        }),
                        Span::new(Rc::clone(&source), 2, 4)
                    ))]
                }),
                Span::new(Rc::clone(&source), 0, 6)
            ))
        )
    }

    #[test]
    fn test_invaild_expr() {
        let sources = vec![
            Source::source("12 + ?"),
            Source::source("12 - ?"),
            Source::source("12 * ?"),
            Source::source("12 / ?"),
            Source::source("12 < ?"),
            Source::source("12 > ?"),
            Source::source("12 <= ?"),
            Source::source("12 >= ?"),
            Source::source("12 == ?"),
            Source::source("12 + 3 + ?"),
            Source::source("12 + 3 - ?"),
            Source::source("12 + 3 * ?"),
            Source::source("12 + 3 / ?"),
        ];

        for source in sources {
            let result = Parser::new(Rc::clone(&source)).parse();
            let err = match result {
                Err(err) => err,
                Ok(_) => unreachable!("The parser should return an error."),
            };
            assert_eq!(
                err.error,
                ExpectedError(ExpectedExpression(
                    "found unexpected token '?'.".to_string()
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
        let first = SyntaxError::new(SyntaxError::UnexpectedError(UnexpectedEOF), &Span::empty());
        let second = ExpectedError(ExpectedExpression(first.error.to_string()));

        let from = parser.make_error(first, second);

        assert_eq!(
            from,
            SyntaxError::new(
                ExpectedError(ExpectedExpression(
                    SyntaxError::UnexpectedError(UnexpectedEOF).to_string(),
                )),
                &Span::empty()
            )
        );

        // If the first is expected, return the the first error.
        let first = SyntaxError::new(
            ExpectedError(ExpectedExpression("found error".to_string())),
            &Span::empty(),
        );
        let second = SyntaxError::UnexpectedError(UnexpectedEOF);

        let from = parser.make_error(first, second);

        assert_eq!(
            from,
            SyntaxError::new(
                ExpectedError(ExpectedExpression("found error".to_string())),
                &Span::empty(),
            )
        );

        // if the both are expected, return the first error.
        let first = SyntaxError::new(
            ExpectedError(ExpectedExpression("found error".to_string())),
            &Span::empty(),
        );
        let second = ExpectedError(ExpectedExpression("found error".to_string()));
        let from = parser.make_error(first, second);

        assert_eq!(
            from,
            SyntaxError::new(
                ExpectedError(ExpectedExpression("found error".to_string())),
                &Span::empty(),
            )
        )
    }
}
*/
