use std::fmt;
use std::collections::HashSet;

use crate::{
    span::Span,
    ast::*,
};

#[derive(Debug, PartialEq, Clone)]
pub struct SemanticError {
    pub error: String,
    pub span: Span,
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let message = format!("{}", self.error);

        write!(f, "Semantic Error: {}\n{}", &message, self.span)
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Symbol {
    VarSymbol,
}

pub struct SymbolTable {
    pub symbols: HashSet<Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable { symbols: HashSet::new() }
    }

    pub fn insert(&mut self, value: Symbol) -> bool {
        self.symbols.insert(value)
    }
}

pub struct SemanticAnalyzer {

}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer {}
    }

    pub fn analyze(&mut self, ast: &AST) {
        for node in &ast.items {
            self.visit(&node)
        }
    }

    fn visit(&mut self, node: &ASTNode) {
        match node {
            ASTNode::Stmt(stmt) => self.statement(&stmt),
            ASTNode::Expr(expr) => self.expression(&expr),
        }
    }

    fn statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::ExpressionStmt(expr, _) => self.visit(&expr.expr),
            Stmt::VarDeclaration(decl, _) => self.var_declaration(&decl),
            Stmt::Assignment(stmt, _) => self.assignment(&stmt),
        }
    }

    fn var_declaration(&mut self, decl: &VarDeclaration) {
        self.visit(&decl.init);

        
    }

    fn assignment(&mut self, stmt: &Assignment) {

    }

    fn expression(&mut self, expr: &Expr) {
        use Expr::*;

        match expr {
            BinaryExpr(expr, _) => self.binary_expression(&expr),
            ParenExpr(expr, _) => self.grouping(&expr),
            UnaryExpr(arg, _) => self.unary(&arg),
            Identifier(id) => self.identifier(&id),
            Literal(_, _) => return,
        }
    }

    fn grouping(&mut self, expr: &ParenExpr) {
        self.visit(&expr.expr);
    }

    fn binary_expression(&mut self, expr: &BinaryExpr) {
        self.visit(&expr.left);
        self.visit(&expr.right);
    }

    fn unary(&mut self, node: &UnaryExpr) {
        self.visit(&node.arg);
    }

    fn identifier(&mut self, id: &Ident) {

    }
}