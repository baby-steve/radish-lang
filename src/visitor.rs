use crate::ast::*;

pub trait Visitor {
    fn visit(&mut self, node: &ASTNode) {
        match node {
            ASTNode::Expr(expr) => self.expression(expr),
            ASTNode::Stmt(stmt) => self.statement(stmt),
        }
    }

    fn statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::ExpressionStmt(expr, _) => self.expression_stmt(&expr),
            Stmt::VarDeclaration(decl, _) => self.var_declaration(&decl),
            Stmt::Assignment(stmt, _) => self.assignment(&stmt),
        }
    }

    fn expression_stmt(&mut self, stmt: &ExpressionStmt) {
        self.visit(&stmt.expr);
    }

    fn var_declaration(&mut self, decl: &VarDeclaration) {
        match &decl.init {
            Some(expr) => self.visit(&expr),
            None => return,
        }
    }

    fn assignment(&mut self, stmt: &Assignment) {
        self.visit(&stmt.expr);
    }

    fn expression(&mut self, expr: &Expr) {
        match expr {
            Expr::BinaryExpr(expr, _) => self.binary_expression(expr),
            Expr::ParenExpr(expr, _) => self.grouping(expr),
            Expr::UnaryExpr(arg, _) => self.unary(arg),
            Expr::Identifier(id) => self.identifier(id),
            Expr::Literal(lit, _) => self.literal(lit),
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

    fn literal(&mut self, node: &Literal) {
        match node {
            Literal::Number(val) => self.number(val),
            Literal::Bool(val) => self.boolean(val),
            Literal::String(val) => self.string(val),
            Literal::Nil => self.nil(),
        }
    }

    fn identifier(&mut self, _: &Ident) {}
    fn number(&mut self, _: &f64) {}
    fn string(&mut self, _: &str) {}
    fn boolean(&mut self, _: &bool) {}
    fn nil(&mut self) {}
}