mod array_expr;
pub use array_expr::ArrayExpr;

mod assignment_expr;
pub use assignment_expr::{AssignmentExpr, OpAssignment};

mod binary_expr;
pub use binary_expr::{BinaryExpr, BinaryOp};

mod call_expr;
pub use call_expr::CallExpr;

mod literal;
pub use literal::{Literal, LiteralKind};

mod logical_expr;
pub use logical_expr::{LogicalExpr, LogicalOp};

mod map_expr;
pub use map_expr::{MapExpr, Property};

mod member_expr;
pub use member_expr::MemberExpr;

mod this_expr;
pub use this_expr::ThisExpr;

mod unary_expr;
pub use unary_expr::{UnaryExpr, UnaryOp};

// mod expr;
pub use super::ast::Expr;

mod block_stmt;
pub use block_stmt::BlockStmt;

mod class_decl;
pub use class_decl::{ClassDecl, MethodDecl, MethodKind};

mod function_decl;
pub use function_decl::FunctionDecl;

mod identifier;
pub use identifier::Identifier;

mod if_stmt;
pub use if_stmt::IfStmt;

mod import_stmt;
pub use import_stmt::ImportStmt;

mod loop_stmt;
pub use loop_stmt::LoopStmt;

mod stmt;
pub use stmt::ExpressionStmt;

mod variable_decl;
pub use variable_decl::{VariableDecl, VariableKind};

mod while_stmt;
pub use while_stmt::WhileStmt;

mod return_stmt;
pub use return_stmt::ReturnStmt;

mod continue_stmt;
pub use continue_stmt::ContinueStmt;

mod break_stmt;
pub use break_stmt::BreakStmt;

use crate::common::Span;
pub use super::ast::Stmt;


pub trait Position {
    fn position(&self) -> &Span;
}
