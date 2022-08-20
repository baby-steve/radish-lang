use serde::Serialize;

use self::identifier::Identifier;

pub use super::*;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Expr {
    MapExpr(MapExpr),
    ArrayExpr(ArrayExpr),
    MemberExpr(MemberExpr),
    LogicalExpr(LogicalExpr),
    BinaryExpr(BinaryExpr),
    UnaryExpr(UnaryExpr),
    CallExpr(CallExpr),
    Identifier(Identifier),
    ThisExpr(ThisExpr),
    Literal(Literal),
}

impl Position for Expr {
    fn position(&self) -> &Span {
        match self {
            Expr::MapExpr(expr) => expr.position(),
            Expr::ArrayExpr(expr) => expr.position(),
            Expr::MemberExpr(expr) => expr.position(),
            Expr::LogicalExpr(expr) => expr.position(),
            Expr::BinaryExpr(expr) => expr.position(),
            Expr::UnaryExpr(expr) => expr.position(),
            Expr::CallExpr(expr) => expr.position(),
            Expr::Identifier(expr) => expr.position(),
            Expr::ThisExpr(expr) => expr.position(),
            Expr::Literal(expr) => expr.position(),
        }
    }
}

impl Analyze for Expr {
    fn analyze(&mut self, analyzer: &mut Analyzer) {
        match self {
            Expr::MapExpr(expr) => expr.analyze(analyzer),
            Expr::ArrayExpr(expr) => expr.analyze(analyzer),
            Expr::MemberExpr(expr) => expr.analyze(analyzer),
            Expr::LogicalExpr(expr) => expr.analyze(analyzer),
            Expr::BinaryExpr(expr) => expr.analyze(analyzer),
            Expr::UnaryExpr(expr) => expr.analyze(analyzer),
            Expr::CallExpr(expr) => expr.analyze(analyzer),
            Expr::Identifier(expr) => expr.analyze(analyzer),
            Expr::ThisExpr(expr) => todo!(),
            Expr::Literal(expr) => expr.analyze(analyzer),
        }
    }
}
