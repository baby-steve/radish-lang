use crate::compiler::ast::*;

pub trait Visitor {
    fn visit(&mut self, node: &ASTNode) {
        match node {
            ASTNode::Expr(expr) => self.expression(expr),
            ASTNode::Stmt(stmt) => self.statement(stmt),
        }
    }

    fn block(&mut self, body: &Vec<Stmt>) {
        for node in body {
            self.statement(&node);
        }
    }

    fn statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::BlockStmt(body, _) => self.block(&body),
            Stmt::ExpressionStmt(expr) => self.expression_stmt(&expr),
            Stmt::VarDeclaration(id, init, _) => self.var_declaration(&id, &init),
            Stmt::Assignment(id, op, expr, _) => self.assignment(&id, &op, &expr),
            Stmt::PrintStmt(expr, _) => self.print(&expr),
        }
    }

    fn var_declaration(&mut self, _: &Ident, init: &Option<Expr>) {
        match &init {
            Some(expr) => self.expression(&expr),
            None => return,
        }
    }

    fn expression_stmt(&mut self, expr: &Expr) {
        self.expression(&expr);
    }

    fn print(&mut self, expr: &Expr) {
        self.expression(&expr);
    }

    fn assignment(&mut self, _: &Ident, _: &OpAssignment, expr: &Expr) {
        self.expression(&expr);
    }

    fn expression(&mut self, expr: &Expr) {
        match expr {
            Expr::BinaryExpr(expr, _) => self.binary_expression(&expr),
            Expr::ParenExpr(expr, _) => self.expression(&expr),
            Expr::UnaryExpr(op, arg, _) => self.unary(&arg, &op),
            Expr::Identifier(id) => self.identifier(&id),
            Expr::Number(num, _) => self.number(&num),
            Expr::String(string, _) => self.string(&string),
            Expr::Bool(val, _) => self.boolean(&val),
            Expr::Nil(_) => self.nil(),
        }
    }

    fn binary_expression(&mut self, expr: &BinaryExpr) {
        self.expression(&expr.left);
        self.expression(&expr.right);
    }

    fn unary(&mut self, arg: &Expr, _: &Op) {
        self.expression(&arg);
    }

    fn identifier(&mut self, _: &Ident) {}
    fn number(&mut self, _: &f64) {}
    fn string(&mut self, _: &str) {}
    fn boolean(&mut self, _: &bool) {}
    fn nil(&mut self) {}
}