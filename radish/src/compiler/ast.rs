//! Module containing Radish's Abstract Syntax Tree (otherwise known as an AST),
//! along with all its related data structures.

use crate::common::Span;

use crate::compiler::scope::ScopeMap;

use std::cmp::Ordering;

use super::hoist::{Scope, VarScope};
use super::SyntaxError;

/// Contains a tree of nested statements and expressions along with
/// any additional information that the frontend/compiler might need.
#[derive(Debug, PartialEq)]
pub struct AST {
    pub items: Vec<Stmt>,
    pub scope: ScopeMap,
    pub other_scope: Option<Scope>,
}

impl AST {
    pub fn new(items: Vec<Stmt>) -> AST {
        AST {
            items,
            scope: ScopeMap::new(),
            other_scope: None,
        }
    }

    pub fn visit<F, T>(&mut self, callback: &mut F) -> Result<T, SyntaxError>
    where
        F: FnMut(&mut AST) -> Result<T, SyntaxError>,
    {
        callback(self)
    }

    pub fn block_stmt(stmts: Vec<Stmt>, span: Span) -> Stmt {
        Stmt::BlockStmt(stmts, span)
    }

    pub fn expr_stmt(expr: Box<Expr>) -> Stmt {
        Stmt::ExpressionStmt(expr)
    }

    pub fn fun_decl(fun: FunctionDecl, span: Span) -> Stmt {
        Stmt::FunDeclaration(fun, span)
    }

    pub fn _con_decl(con: ConstructorDecl, span: Span) -> Stmt {
        Stmt::ConDeclaration(con, span)
    }

    pub fn class_decl(class: ClassDecl, span: Span) -> Stmt {
        Stmt::ClassDeclaration(class, span)
    }

    pub fn var_decl(id: Ident, expr: Option<Expr>, kind: VarKind, span: Span) -> Stmt {
        let stmt = VarDeclaration {
            name: id,
            init: expr,
            kind,
        };

        Stmt::VarDeclaration(stmt, span)
    }

    pub fn assignment(stmt: AssignmentStmt, span: Span) -> Stmt {
        Stmt::AssignmentStmt(stmt, span)
    }

    pub fn if_stmt(condition: Expr, block: Vec<Stmt>, alt: Option<Box<Stmt>>, span: Span) -> Stmt {
        Stmt::IfStmt(condition, block, alt, span)
    }

    pub fn loop_stmt(block: Vec<Stmt>, span: Span) -> Stmt {
        Stmt::LoopStmt(block, span)
    }

    pub fn while_stmt(condition: Expr, block: Vec<Stmt>, span: Span) -> Stmt {
        Stmt::WhileStmt(condition, block, span)
    }

    pub fn import_stmt(path: String, items: Vec<Ident>, span: Span) -> Stmt {
        Stmt::ImportStmt(ImportStatement::new(path, items, span))
    }

    pub fn break_stmt(span: Span) -> Stmt {
        Stmt::BreakStmt(span)
    }

    pub fn continue_stmt(span: Span) -> Stmt {
        Stmt::ContinueStmt(span)
    }

    pub fn return_stmt(return_expr: Option<Expr>, span: Span) -> Stmt {
        Stmt::ReturnStmt(return_expr, span)
    }

    pub fn print_stmt(expr: Expr, span: Span) -> Stmt {
        Stmt::PrintStmt(expr, span)
    }

    pub fn array(elements: Vec<Expr>, span: Span) -> Expr {
        Expr::ArrayExpr(elements, span)
    }

    pub fn map(values: Vec<Expr>, span: Span) -> Expr {
        Expr::MapExpr(values, span)
    }

    pub fn binary_expr(expr: Box<BinaryExpr>, span: Span) -> Expr {
        Expr::BinaryExpr(expr, span)
    }

    pub fn paren_expr(expr: Box<Expr>, span: Span) -> Expr {
        Expr::ParenExpr(expr, span)
    }

    pub fn unary_expr(op: Op, expr: Box<Expr>, span: Span) -> Expr {
        Expr::UnaryExpr(op, expr, span)
    }

    pub fn logical_expr(expr: Box<BinaryExpr>, span: Span) -> Expr {
        Expr::LogicalExpr(expr, span)
    }

    pub fn call_expr(expr: Box<Expr>, args: Vec<Expr>, span: Span) -> Expr {
        Expr::CallExpr(expr, args, span)
    }

    pub fn member_expr(obj: Box<Expr>, prop: Box<Expr>, span: Span) -> Expr {
        Expr::MemberExpr(obj, prop, span)
    }

    pub fn identifier(id: Ident) -> Expr {
        Expr::Identifier(id)
    }

    pub fn string(str: String, span: Span) -> Expr {
        Expr::String(str, span)
    }

    pub fn number(num: f64, span: Span) -> Expr {
        Expr::Number(num, span)
    }

    pub fn bool(val: bool, span: Span) -> Expr {
        Expr::Bool(val, span)
    }

    pub fn nil(span: Span) -> Expr {
        Expr::Nil(span)
    }
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum Stmt {
    /// A block statement
    /// ```txt
    /// '{' <expr>... '}'
    /// ```
    BlockStmt(Vec<Stmt>, Span),
    /// An expression statement.
    /// ```txt
    /// <expr>
    /// ```
    ExpressionStmt(Box<Expr>),
    /// A function defintion.
    /// ```txt
    /// 'fun' <ident> '(' param... ')' '{' stmt... '}'
    /// ```
    FunDeclaration(FunctionDecl, Span),
    /// A class declaration.
    /// ```txt
    /// 'class' <ident> '{' stmt... '}'
    /// ```
    ClassDeclaration(ClassDecl, Span),
    /// A class constructor declaration.
    /// ```txt
    /// 'con' <ident> '(' param... ')' '{' stmt... '}'
    /// ```
    ConDeclaration(ConstructorDecl, Span),
    /// A variable declaration.
    /// ```txt
    /// ('var'|'fin') <ident> ['=' <expr>]
    /// ```
    VarDeclaration(VarDeclaration, Span),
    /// An assignment statement.
    /// ```txt
    /// <ident> <op>'=' <expr>
    /// ```
    AssignmentStmt(AssignmentStmt, Span),
    /// An `if` statement.
    /// ```txt
    /// 'if' <expr> 'then' stmt... ['else' stmt...] 'endif'
    /// ```
    IfStmt(Expr, Vec<Stmt>, Option<Box<Stmt>>, Span),
    /// A `loop` statement.
    /// ```txt
    /// 'loop' stmt... 'endloop'
    /// ```
    LoopStmt(Vec<Stmt>, Span),
    /// A `while` loop statement.
    /// ```txt
    /// 'while' <expr> 'loop' stmt... 'endloop'
    /// ```
    WhileStmt(Expr, Vec<Stmt>, Span),
    /// A `import` statement
    /// ```txt
    /// 'import' <path> { 'for' ident... }
    /// ```
    ImportStmt(ImportStatement),
    //ImportStmt(String, Vec<Ident>, Span),
    /// A break statement.
    /// ```txt
    /// break
    /// ```
    BreakStmt(Span),
    /// A continue statement
    /// ```txt
    /// continue
    /// ```
    ContinueStmt(Span),
    /// A return statement
    /// ```txt
    /// return [<expr>]
    /// ```
    ReturnStmt(Option<Expr>, Span),
    /// A print statement. Note: this is a temporary language
    /// construct and will eventually be replaced by functions in the stdlib.
    /// ```txt
    /// 'print' <expr>
    /// ```
    PrintStmt(Expr, Span),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VarKind {
    Var,
    Fin,
}

impl Stmt {
    pub fn into_var_decl(self) -> (VarDeclaration, Span) {
        match self {
            Stmt::VarDeclaration(decl, span) => (decl, span),
            _ => panic!("not a variable declaration"),
        }
    }
}

/// An assignment statement.
#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentStmt {
    pub lhs: Expr,
    pub op: OpAssignment,
    pub rhs: Expr,
}

impl AssignmentStmt {
    pub fn new(op: OpAssignment, lhs: Expr, rhs: Expr) -> Self {
        Self { op, lhs, rhs }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDeclaration {
    pub name: Ident,
    pub init: Option<Expr>,
    pub kind: VarKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// An array literal.
    ArrayExpr(Vec<Expr>, Span),
    /// A map literal.
    MapExpr(Vec<Expr>, Span),
    /// A binary expression
    /// ```txt
    /// <expr> <op> <expr>
    /// ```
    BinaryExpr(Box<BinaryExpr>, Span),
    /// A grouping expression
    /// ```txt
    /// '(' <expr> ')'
    /// ```
    ParenExpr(Box<Expr>, Span),
    /// An unary expression
    /// ```txt
    /// <op> <expr>
    /// ```
    UnaryExpr(Op, Box<Expr>, Span),
    /// A logical expression
    /// ```txt
    /// <expr> 'and'|'or' <expr>
    /// ```
    LogicalExpr(Box<BinaryExpr>, Span),
    /// A call expression
    /// ```txt
    /// <callee> '(' <args> ')'
    /// ```
    CallExpr(Box<Expr>, Vec<Expr>, Span),
    /// A member expression
    /// ```txt
    /// <object> '.' <property>
    /// ```
    MemberExpr(Box<Expr>, Box<Expr>, Span),
    /// An identifier
    Identifier(Ident),
    /// A number literal
    Number(f64, Span),
    /// A boolean literal
    /// ```txt
    /// true | false
    /// ```
    Bool(bool, Span),
    /// A string literal
    String(String, Span),
    /// `nil` literal.
    Nil(Span),
}

impl Expr {
    pub fn position(&self) -> Span {
        match self {
            Self::ArrayExpr(_, pos)
            | Self::MapExpr(_, pos)
            | Self::BinaryExpr(_, pos)
            | Self::ParenExpr(_, pos)
            | Self::UnaryExpr(_, _, pos)
            | Self::LogicalExpr(_, pos)
            | Self::CallExpr(_, _, pos)
            | Self::MemberExpr(_, _, pos)
            | Self::Number(_, pos)
            | Self::Bool(_, pos)
            | Self::String(_, pos)
            | Self::Nil(pos) => pos.clone(),
            Self::Identifier(id) => id.pos.clone(),
        }
    }
}

/// A binary expression.
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    /// Left hand side of the expression.
    pub lhs: Expr,
    /// Expression's operand.
    pub op: Op,
    /// Right hand side of the expression.
    pub rhs: Expr,
}

impl BinaryExpr {
    pub fn new(op: Op, l: Expr, r: Expr) -> BinaryExpr {
        BinaryExpr { op, lhs: l, rhs: r }
    }
}

/// A function declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl {
    /// The function's name
    pub id: Ident,
    /// The function's parameter list.
    pub params: Vec<Ident>,
    /// The body of the function.
    pub body: Vec<Stmt>,
    /// This function's scope map.
    pub scope: ScopeMap,
    pub other_scope: Option<Scope>,
}

impl FunctionDecl {
    pub fn new(id: Ident, params: Vec<Ident>, body: Vec<Stmt>) -> FunctionDecl {
        FunctionDecl {
            id,
            params,
            body,
            scope: ScopeMap::new(),
            other_scope: None,
        }
    }
}

/// A class declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct ClassDecl {
    /// the name of the class
    pub id: Ident,
    /// this class's constructors
    pub constructors: Vec<ConstructorDecl>,
    pub fields: Vec<VarDeclaration>,
}

impl ClassDecl {
    pub fn new(
        id: Ident,
        constructors: Vec<ConstructorDecl>,
        fields: Vec<VarDeclaration>,
    ) -> ClassDecl {
        ClassDecl {
            id,
            constructors,
            fields,
        }
    }
}

/// A constructor declaration
/// ```text
/// 'con' <id> '(' <params> ')' '{' <body> '}'
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct ConstructorDecl {
    /// constructor name
    pub id: Ident,
    /// constructor's parameter list
    pub params: Vec<Ident>,
    /// body of the constructor
    pub body: Box<Vec<Stmt>>,
}

impl ConstructorDecl {
    pub fn new(id: Ident, params: Vec<Ident>, body: Box<Vec<Stmt>>) -> Self {
        ConstructorDecl { id, params, body }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportStatement {
    path: String,
    items: Vec<Ident>,
    pos: Span,
}

impl ImportStatement {
    pub fn new(path: String, items: Vec<Ident>, pos: Span) -> Self {
        Self { path, items, pos }
    }

    pub fn name(&self) -> Option<Ident> {
        use std::path::Path;

        let path = Path::new(&self.path);

        let file_path = path.file_name();

        file_path.map(|p| p.to_str().unwrap()).map(|name| {
            let span = self.pos.clone();
            Ident::new(name.to_string(), span)
        })
    }

    pub fn path(&self) -> &str {
        &self.path
    }
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub name: String,
    pub pos: Span,
    pub index: u32,
    pub scope: VarScope,
}

impl Ident {
    pub fn new(name: String, pos: Span) -> Self {
        Self {
            name,
            pos,
            index: 0,
            scope: VarScope::default(),
        }
    }
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

/// A binary or unary operand.
#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
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

/// An assignment operand.
#[derive(Debug, Clone, PartialEq)]
pub enum OpAssignment {
    Equals,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
}
