use crate::span::Span;

#[derive(Debug, PartialEq)]
pub struct AST {
    pub items: Vec<ASTNode>,
}

impl AST {
    pub fn new(items: Vec<ASTNode>) -> AST {
        AST { items }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTNode {
    Expr(Expr),
    Stmt(Stmt),
}

impl ASTNode {
    pub fn position(&self) -> Span {
        match self {
            Self::Expr(expr) => expr.position(), 
            Self::Stmt(stmt) => stmt.position(), 
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    ExpressionStmt(Box<ExpressionStmt>, Span),
}

impl Stmt {
    pub fn position(&self) -> Span {
        match self {
            Self::ExpressionStmt(_, pos) => pos.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStmt {
    pub expr: ASTNode,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    BinaryExpr(Box<BinaryExpr>, Span),
    ParenExpr(Box<ParenExpr>, Span),
    UnaryExpr(Box<UnaryExpr>, Span),
    Literal(Literal, Span),
}

impl Expr {
    pub fn position(&self) -> Span {
        match self {
            Self::BinaryExpr(_, pos) 
            | Self::ParenExpr(_, pos)
            | Self::UnaryExpr(_, pos)
            | Self::Literal(_, pos) => pos.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub left: ASTNode,
    pub op: Op,
    pub right: ASTNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParenExpr {
    pub expr: ASTNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub op: Op,
    pub arg: ASTNode,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(f64),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
}