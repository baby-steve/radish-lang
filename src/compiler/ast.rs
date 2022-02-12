use crate::common::span::Span;
use crate::SymbolTable;
use std::cmp::Ordering;

#[derive(Debug, PartialEq)]
pub struct AST {
    pub items: Vec<Stmt>,
}

impl AST {
    pub fn new(items: Vec<Stmt>) -> AST {
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
    // <expr>*
    BlockStmt(Box<Vec<Stmt>>, Span),
    // <expr>
    ExpressionStmt(Box<Expr>),
    // <Function>
    FunDeclaration(Function, Span),
    // 'var' <id> '=' <expr>
    VarDeclaration(Ident, Option<Expr>, Span),
    // <id> <op> <expr>
    Assignment(Ident, OpAssignment, Expr, Span),
    // if <expr> <block> <alternate> end
    IfStmt(Expr, Box<Vec<Stmt>>, Option<Box<Stmt>>, Span),
    // loop <block> endloop
    LoopStmt(Box<Vec<Stmt>>, Span),
    // while <expr> loop <block> endloop
    WhileStmt(Expr, Box<Vec<Stmt>>, Span),
    // break
    BreakStmt(Span),
    // continue
    ContinueStmt(Span),
    // return <expr>?
    ReturnStmt(Option<Expr>, Span),
    // 'print' <expr>
    PrintStmt(Expr, Span),
}

impl Stmt {
    pub fn position(&self) -> Span {
        match self {
            Self::VarDeclaration(_, _, pos)
            | Self::FunDeclaration(_, pos)
            | Self::PrintStmt(_, pos)
            | Self::BlockStmt(_, pos)
            | Self::Assignment(_, _, _, pos)
            | Self::IfStmt(_, _, _, pos)
            | Self::LoopStmt(_, pos)
            | Self::WhileStmt(_, _, pos)
            | Self::ReturnStmt(_, pos)
            | Self::ContinueStmt(pos)
            | Self::BreakStmt(pos) => pos.clone(),
            Self::ExpressionStmt(expr) => expr.position(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    BinaryExpr(Box<BinaryExpr>, Span),
    ParenExpr(Box<Expr>, Span),
    UnaryExpr(Op, Box<Expr>, Span),
    LogicalExpr(Box<BinaryExpr>, Span),
    CallExpr(Box<Expr>, Vec<Box<Expr>>, Span),
    Identifier(Ident),
    Number(f64, Span),
    Bool(bool, Span),
    String(String, Span),
    Nil(Span),
}

impl Expr {
    pub fn position(&self) -> Span {
        match self {
            Self::BinaryExpr(_, pos)
            | Self::ParenExpr(_, pos)
            | Self::UnaryExpr(_, _, pos)
            | Self::LogicalExpr(_, pos)
            | Self::CallExpr(_, _, pos)
            | Self::Number(_, pos)
            | Self::Bool(_, pos)
            | Self::String(_, pos)
            | Self::Nil(pos) => pos.clone(),
            Self::Identifier(id) => id.pos.clone(),
        }
    }

    pub fn is_callable(&self) -> bool {
        match self {
            Self::Identifier(_) | Self::CallExpr(_, _, _) => true,
            Self::UnaryExpr(_, arg, _) => arg.is_callable(),
            _ => false,
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
    pub left: Expr,
    pub op: Op,
    pub right: Expr,
}

impl BinaryExpr {
    pub fn new(op: Op, l: Expr, r: Expr) -> BinaryExpr {
        BinaryExpr {
            op,
            left: l,
            right: r,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub id: Ident,
    pub params: Vec<Ident>,
    pub body: Box<Vec<Stmt>>,
    // HACK
    pub scope: SymbolTable,
}

impl Function {
    pub fn new(id: Ident, params: Vec<Ident>, body: Box<Vec<Stmt>>) -> Function {
        Function {
            id,
            params,
            body,
            scope: SymbolTable::new(0), // HACK
        }
    }
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub name: String,
    pub pos: Span,
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl PartialOrd for Ident {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Ident {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(&other.name)
    }
}

impl Eq for Ident {}

impl std::hash::Hash for Ident {
    fn hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        self.name.hash(state);
        state.finish();
    }
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
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OpAssignment {
    Equals,
    PlusEquals,
    MinusEquals,
    MultiplyEquals,
    DivideEquals,
}
