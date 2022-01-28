use crate::compiler::ast::*;

// Todo: should only have defaults for some of these.
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
            Stmt::FunDeclaration(fun, _) => self.function_declaration(fun),
            Stmt::VarDeclaration(id, init, _) => self.var_declaration(&id, &init),
            Stmt::Assignment(id, op, expr, _) => self.assignment(&id, &op, &expr),
            Stmt::IfStmt(expr, body, alt, _) => self.if_statement(&expr, &body, &alt),
            Stmt::LoopStmt(body, _) => self.loop_statement(&body),
            Stmt::WhileStmt(expr, body, _) => self.while_statement(expr, body),
            Stmt::Break(_) => self.break_statement(),
            Stmt::Continue(_) => self.continue_statement(),
            Stmt::PrintStmt(expr, _) => self.print(&expr),
        }
    }

    fn function_declaration(&mut self, fun: &Function) {
        self.block(&fun.body);
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

    fn if_statement(&mut self, expr: &Expr, body: &Vec<Stmt>, else_branch: &Option<Box<Stmt>>) {
        self.expression(&expr);
        self.block(&body);
        if let Some(else_branch) = &else_branch {
            self.statement(&else_branch);
        }
    }

    fn loop_statement(&mut self, body: &Vec<Stmt>) {
        self.block(&body);
    }

    fn while_statement(&mut self, expr: &Expr, body: &Vec<Stmt>) {
        self.expression(&expr);
        self.block(&body);
    }

    fn break_statement(&mut self) {}

    fn continue_statement(&mut self) {}

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
            Expr::LogicalExpr(expr, _) => self.logical_expr(&expr),
            Expr::CallExpr(callee, args, _) => self.call_expr(&callee, &args),
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

    fn logical_expr(&mut self, expr: &BinaryExpr) {
        self.expression(&expr.left);
        self.expression(&expr.right);
    }

    fn unary(&mut self, arg: &Expr, _: &Op) {
        self.expression(&arg);
    }

    fn call_expr(&mut self, _: &Expr, _: &Vec<Box<Expr>>) {}

    fn identifier(&mut self, _: &Ident) {}
    fn number(&mut self, _: &f64) {}
    fn string(&mut self, _: &str) {}
    fn boolean(&mut self, _: &bool) {}
    fn nil(&mut self) {}
}
