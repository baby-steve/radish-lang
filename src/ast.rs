use crate::token::Span;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(f64),
}

#[derive(Debug, PartialEq)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
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

impl fmt::Display for BinaryExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{{ left: {:?}, op: {:?}, right: {:?}, }}",
            self.left, self.op, self.right
        )
    }
}

#[derive(Debug, PartialEq)]
pub enum ASTNode {
    BinaryExpr(Box<BinaryExpr>, Span),
    ParenExpr(Box<ParenExpr>, Span),
    Literal(Literal, Span),
}

impl ASTNode {
    pub fn position(&self) -> Span {
        match self {
            Self::BinaryExpr(_, pos) 
            | Self::ParenExpr(_, pos)
            | Self::Literal(_, pos) => *pos,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct AST {
    pub items: Vec<ASTNode>,
}

impl AST {
    pub fn new(items: Vec<ASTNode>) -> AST {
        AST { items }
    }
}
