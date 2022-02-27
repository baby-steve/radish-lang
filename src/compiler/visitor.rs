use crate::common::span::Span;
use crate::compiler::ast::*;

// Todo: should only have defaults for some of these.
pub trait Visitor<E, T> {
    fn visit(&mut self, node: &ASTNode) -> Result<E, T> {
        match node {
            ASTNode::Expr(expr) => self.expression(expr),
            ASTNode::Stmt(stmt) => self.statement(stmt),
        }
    }

    fn block(&mut self, body: &Vec<Stmt>) -> Result<E, T>;

    fn statement(&mut self, stmt: &Stmt) -> Result<E, T> {
        match stmt {
            Stmt::BlockStmt(body, _) => self.block(&body),
            Stmt::ExpressionStmt(expr) => self.expression_stmt(&expr),
            Stmt::FunDeclaration(fun, _) => self.function_declaration(&fun),
            Stmt::ConDeclaration(con, _) => self.constructor_declaration(&con),
            Stmt::ClassDeclaration(class, _) => self.class_declaration(class),
            Stmt::VarDeclaration(id, init, kind, _) => self.var_declaration(&id, &init, &kind),
            Stmt::Assignment(id, op, expr, _) => self.assignment(&id, &op, &expr),
            Stmt::IfStmt(expr, body, alt, _) => self.if_statement(&expr, &body, &alt),
            Stmt::LoopStmt(body, _) => self.loop_statement(&body),
            Stmt::WhileStmt(expr, body, _) => self.while_statement(&expr, &body),
            Stmt::ReturnStmt(value, _) => self.return_statement(&value),
            Stmt::BreakStmt(pos) => self.break_statement(&pos),
            Stmt::ContinueStmt(pos) => self.continue_statement(&pos),
            Stmt::PrintStmt(expr, _) => self.print(&expr),
        }
    }

    fn function_declaration(&mut self, fun: &FunctionDecl) -> Result<E, T>;

    fn class_declaration(&mut self, class: &ClassDecl) -> Result<E, T>;

    fn constructor_declaration(&mut self, con: &ConstructorDecl) -> Result<E, T>;

    fn var_declaration(&mut self, _: &Ident, init: &Option<Expr>, kind: &VarKind) -> Result<E, T>;

    fn expression_stmt(&mut self, expr: &Expr) -> Result<E, T> {
        self.expression(&expr)
    }

    fn if_statement(
        &mut self,
        expr: &Expr,
        body: &Vec<Stmt>,
        else_branch: &Option<Box<Stmt>>,
    ) -> Result<E, T> {
        self.expression(&expr)?;
        if let Some(else_branch) = &else_branch {
            self.statement(&else_branch)?;
        }
        self.block(&body)
    }

    fn loop_statement(&mut self, body: &Vec<Stmt>) -> Result<E, T> {
        self.block(&body)
    }

    fn while_statement(&mut self, expr: &Expr, body: &Vec<Stmt>) -> Result<E, T> {
        self.expression(&expr)?;
        self.block(&body)
    }

    fn return_statement(&mut self, _: &Option<Expr>) -> Result<E, T>;
    fn break_statement(&mut self, pos: &Span) -> Result<E, T>;
    fn continue_statement(&mut self, pos: &Span) -> Result<E, T>;

    fn print(&mut self, expr: &Expr) -> Result<E, T> {
        self.expression(&expr)
    }

    fn assignment(&mut self, _: &Ident, _: &OpAssignment, expr: &Expr) -> Result<E, T> {
        self.expression(&expr)
    }

    fn expression(&mut self, expr: &Expr) -> Result<E, T> {
        match expr {
            Expr::BinaryExpr(expr, _) => self.binary_expression(&expr),
            Expr::ParenExpr(expr, _) => self.expression(&expr),
            Expr::UnaryExpr(op, arg, _) => self.unary(&arg, &op),
            Expr::LogicalExpr(expr, _) => self.logical_expr(&expr),
            Expr::CallExpr(callee, args, _) => self.call_expr(&callee, &args),
            Expr::MemberExpr(obj, prop, _) => self.member_expr(&obj, &prop),
            Expr::Identifier(id) => self.identifier(&id),
            Expr::Number(num, _) => self.number(&num),
            Expr::String(string, _) => self.string(&string),
            Expr::Bool(val, _) => self.boolean(&val),
            Expr::Nil(_) => self.nil(),
        }
    }

    fn binary_expression(&mut self, expr: &BinaryExpr) -> Result<E, T> {
        self.expression(&expr.left)?;
        self.expression(&expr.right)
    }

    fn logical_expr(&mut self, expr: &BinaryExpr) -> Result<E, T> {
        self.expression(&expr.left)?;
        self.expression(&expr.right)
    }

    fn unary(&mut self, arg: &Expr, _: &Op) -> Result<E, T> {
        self.expression(&arg)
    }

    fn call_expr(&mut self, _: &Expr, _: &Vec<Box<Expr>>) -> Result<E, T>;
    fn member_expr(&mut self, _: &Expr, _: &Expr) -> Result<E, T>;
    fn identifier(&mut self, _: &Ident) -> Result<E, T>;
    fn number(&mut self, _: &f64) -> Result<E, T>;
    fn string(&mut self, _: &str) -> Result<E, T>;
    fn boolean(&mut self, _: &bool) -> Result<E, T>;
    fn nil(&mut self) -> Result<E, T>;
}
