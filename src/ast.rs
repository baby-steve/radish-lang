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

#[derive(Debug, PartialEq)]
pub enum ASTNode {
    Expr(Expr),
}

impl ASTNode {
    pub fn position(&self) -> Span {
        match self {
            Self::Expr(expr) => expr.position(), 
        }
    }
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub struct BinaryExpr {
    pub left: ASTNode,
    pub op: Op,
    pub right: ASTNode,
}

#[derive(Debug, PartialEq)]
pub struct ParenExpr {
    pub expr: ASTNode,
}

#[derive(Debug, PartialEq)]
pub struct UnaryExpr {
    pub op: Op,
    pub arg: ASTNode,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(f64),
    Bool(bool),
}

#[derive(Debug, PartialEq)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
}