use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub name: String,
    pub pos: Span,
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

#[derive(Debug, Clone, PartialEq)]
pub enum ASTNode {
    Expr(Expr),
    Stmt(Stmt),
}

impl From<Stmt> for ASTNode {
    fn from(stmt: Stmt) -> Self {
        ASTNode::Stmt(stmt)
    }
}

impl From<Expr> for ASTNode {
    fn from(expr: Expr) -> Self {
        ASTNode::Expr(expr)
    }
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
    VarDeclaration(Box<VarDeclaration>, Span),
    Assignment(Box<Assignment>, Span),
}

impl Stmt {
    pub fn position(&self) -> Span {
        match self {
            Self::VarDeclaration(_, pos)
            | Self::Assignment(_, pos)
            | Self::ExpressionStmt(_, pos) => pos.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStmt {
    pub expr: ASTNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDeclaration {
    pub id: Ident,
    pub init: ASTNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub id: Ident,
    pub op: OpAssignment,
    pub expr: ASTNode,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    BinaryExpr(Box<BinaryExpr>, Span),
    ParenExpr(Box<ParenExpr>, Span),
    UnaryExpr(Box<UnaryExpr>, Span),
    Identifier(Ident),
    Literal(Literal, Span),
}

impl Expr {
    pub fn position(&self) -> Span {
        match self {
            Self::BinaryExpr(_, pos) 
            | Self::ParenExpr(_, pos)
            | Self::UnaryExpr(_, pos)
            | Self::Literal(_, pos) => pos.clone(),
            
            Self::Identifier(id) => id.pos.clone(),
        }
    }

    pub fn is_ident(&self) -> bool {
        match self {
            Self::Identifier(_) => true,
            _ => false,
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
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
    Bang,
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
    EqualsTo,
    NotEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OpAssignment {
    Equals,
}