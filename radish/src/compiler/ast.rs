//! Module containing Radish's Abstract Syntax Tree (otherwise known as an AST).
//! 

use serde::Serialize;

use crate::common::Span;

use crate::compiler::scope::ScopeMap;

use super::ast_ast::{
    ArrayExpr, BinaryExpr, BlockStmt, BreakStmt, CallExpr, ContinueStmt, ExpressionStmt,
    FunctionDecl, Identifier, IfStmt, ImportStmt, Literal, LogicalExpr, LoopStmt, MapExpr,
    MemberExpr, Position, Property, ReturnStmt, ThisExpr, UnaryExpr, UnaryOp, VariableDecl,
    WhileStmt, AssignmentExpr, MethodDecl, ClassDecl,
};
use super::hoist::Scope;
use super::SyntaxError;

/// Contains a tree of nested statements and expressions along with
/// any additional information that the frontend/compiler might need.
#[derive(Debug, PartialEq, Serialize)]
pub struct AST {
    pub items: Vec<Stmt>,
    #[serde(skip_serializing)]
    pub scope: ScopeMap,
    #[serde(skip_serializing)]
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

    pub fn if_stmt(
        condition: Expr,
        consequent: BlockStmt,
        alternate: Option<Box<Stmt>>,
        span: Span,
    ) -> Stmt {
        let if_stmt = IfStmt {
            condition,
            consequent,
            alternate,
            span,
        };

        Stmt::IfStmt(if_stmt)
    }

    pub fn loop_stmt(body: BlockStmt, span: Span) -> Stmt {
        let loop_stmt = LoopStmt { body, span };

        Stmt::LoopStmt(loop_stmt)
    }

    pub fn while_stmt(condition: Expr, body: BlockStmt, span: Span) -> Stmt {
        let while_stmt = WhileStmt {
            condition,
            body,
            span,
        };

        Stmt::WhileStmt(while_stmt)
    }

    pub fn import_stmt(path: String, _items: Vec<Identifier>, span: Span) -> Stmt {
        Stmt::ImportStmt(ImportStmt { source: path, span })
    }

    pub fn break_stmt(span: Span) -> Stmt {
        Stmt::BreakStmt(BreakStmt { span })
    }

    pub fn continue_stmt(span: Span) -> Stmt {
        Stmt::ContinueStmt(ContinueStmt { span })
    }

    pub fn return_stmt(return_expr: Option<Expr>, span: Span) -> Stmt {
        let return_stmt = ReturnStmt {
            argument: return_expr,
            span,
        };

        Stmt::ReturnStmt(return_stmt)
    }

    pub fn print_stmt(expr: Expr, span: Span) -> Stmt {
        Stmt::PrintStmt(expr, span)
    }

    pub fn array(elements: Vec<Expr>, span: Span) -> Expr {
        let array_expr = ArrayExpr { elements, span };

        Expr::ArrayExpr(array_expr)
    }

    pub fn map(properties: Vec<Property>, span: Span) -> Expr {
        let map_expr = MapExpr { properties, span };

        Expr::MapExpr(map_expr)
    }

    pub fn binary_expr(expr: BinaryExpr) -> Expr {
        Expr::BinaryExpr(expr)
    }

    pub fn unary_expr(op: UnaryOp, argument: Box<Expr>, span: Span) -> Expr {
        let unary_expr = UnaryExpr { op, argument, span };

        Expr::UnaryExpr(unary_expr)
    }

    pub fn call_expr(expr: Box<Expr>, args: Vec<Expr>, span: Span) -> Expr {
        let call_expr = CallExpr {
            callee: expr,
            args,
            span,
        };

        Expr::CallExpr(call_expr)
    }

    pub fn member_expr(object: Box<Expr>, property: Box<Expr>, computed: bool, span: Span) -> Expr {
        let member_expr = MemberExpr {
            object,
            property,
            computed,
            span,
        };

        Expr::MemberExpr(member_expr)
    }

    pub fn this(span: Span) -> Expr {
        Expr::This(ThisExpr { span })
    }

    pub fn identifier(id: Identifier) -> Expr {
        Expr::Identifier(id)
    }

    pub fn string(str: String, span: Span) -> Expr {
        Expr::Literal(Literal::string(str, span))
    }

    pub fn number(num: f64, span: Span) -> Expr {
        Expr::Literal(Literal::number(num, span))
    }

    pub fn bool(val: bool, span: Span) -> Expr {
        Expr::Literal(Literal::boolean(val, span))
    }

    pub fn nil(span: Span) -> Expr {
        Expr::Literal(Literal::nil(span))
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Stmt {
    /// A block statement
    /// ```txt
    /// '{' <expr>... '}'
    /// ```
    BlockStmt(BlockStmt),
    /// An expression statement.
    /// ```txt
    /// <expr>
    /// ```
    ExpressionStmt(ExpressionStmt),
    /// A function defintion.
    /// ```txt
    /// 'fun' <ident> '(' param... ')' '{' stmt... '}'
    /// ```
    FunctionDecl(FunctionDecl),
    /// A class method or constructor declaration.
    /// ```txt
    /// ('fun' | 'con') <ident> '(' param... ')' '{' stmt... '}' 
    /// ```
    MethodDecl(MethodDecl),
    /// A class declaration.
    /// ```txt
    /// 'class' <ident> '{' stmt... '}'
    /// ```
    ClassDecl(ClassDecl),
    /// A variable declaration.
    /// ```txt
    /// ('var'|'fin') <ident> ['=' <expr>]
    /// ```
    VariableDecl(VariableDecl),
    /// An `if` statement.
    /// ```txt
    /// 'if' <expr> 'then' stmt... ['else' stmt...] 'endif'
    /// ```
    IfStmt(IfStmt),
    /// A `loop` statement.
    /// ```txt
    /// 'loop' stmt... 'endloop'
    /// ```
    LoopStmt(LoopStmt),
    /// A `while` loop statement.
    /// ```txt
    /// 'while' <expr> 'loop' stmt... 'endloop'
    /// ```
    WhileStmt(WhileStmt),
    /// A `import` statement
    /// ```txt
    /// 'import' <path> { 'for' ident... }
    /// ```
    ImportStmt(ImportStmt),
    /// A break statement.
    /// ```txt
    /// break
    /// ```
    BreakStmt(BreakStmt),
    /// A continue statement
    /// ```txt
    /// continue
    /// ```
    ContinueStmt(ContinueStmt),
    /// A return statement
    /// ```txt
    /// return [<expr>]
    /// ```
    ReturnStmt(ReturnStmt),
    /// A print statement. Note: this is a temporary language
    /// construct and will eventually be replaced by functions in the stdlib.
    /// ```txt
    /// 'print' <expr>
    /// ```
    PrintStmt(Expr, Span),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Expr {
    /// An array literal.
    ArrayExpr(ArrayExpr),
    /// A map literal.
    MapExpr(MapExpr),
    /// An assignment expression.
    AssignmentExpr(AssignmentExpr),
    /// A binary expression
    /// ```txt
    /// <expr> <op> <expr>
    /// ```
    BinaryExpr(BinaryExpr),
    /// An unary expression
    /// ```txt
    /// <op> <expr>
    /// ```
    UnaryExpr(UnaryExpr),
    /// A logical expression
    /// ```txt
    /// <expr> 'and'|'or' <expr>
    /// ```
    LogicalExpr(LogicalExpr),
    /// A call expression
    /// ```txt
    /// <callee> '(' <args> ')'
    /// ```
    CallExpr(CallExpr),
    /// A member expression
    /// ```txt
    /// <object> '.' <property>
    /// ```
    MemberExpr(MemberExpr),
    Identifier(Identifier),
    This(ThisExpr),
    Literal(Literal),
}

impl Position for Expr {
    fn position(&self) -> &Span {
        match self {
            Self::BinaryExpr(expr) => expr.position(),
            Self::LogicalExpr(expr) => expr.position(),
            Self::AssignmentExpr(expr) => expr.position(),
            Self::MemberExpr(expr) => expr.position(),
            Self::UnaryExpr(expr) => expr.position(),
            Self::MapExpr(expr) => expr.position(),
            Self::ArrayExpr(expr) => expr.position(),
            Self::CallExpr(expr) => expr.position(),
            Self::Literal(lit) => lit.position(),
            Self::This(expr) => expr.position(),
            Self::Identifier(id) => id.position(),
        }
    }
}
