//! Module containing an AST visitor.

use crate::compiler::ast::*;

use super::SyntaxError;

/// Alias for the Visitor method's return type.
pub type VisitorResult = Result<(), SyntaxError>;

/// Transverses an AST's nodes. By default, it recursively walks the
/// tree and does absolutely nothing.
pub trait Visitor<'a>: Sized {
    fn visit_stmt(&mut self, stmt: &mut Stmt) -> VisitorResult {
        match stmt {
            Stmt::BlockStmt(block, _) => self.visit_block_stmt(block),
            Stmt::ExpressionStmt(expr) => self.visit_expr_stmt(expr),
            Stmt::FunDeclaration(fun, _) => self.visit_fun_decl(fun),
            Stmt::ConDeclaration(con, _) => self.visit_con_decl(con),
            Stmt::ClassDeclaration(class, _) => self.visit_class_decl(class),
            Stmt::VarDeclaration(stmt, _) => self.visit_var_decl(stmt),
            Stmt::AssignmentStmt(stmt, _) => self.visit_assignment(stmt),
            Stmt::IfStmt(condition, body, alt, _) => self.visit_if_stmt(condition, body, alt),
            Stmt::LoopStmt(body, _) => self.visit_loop_stmt(body),
            Stmt::WhileStmt(condition, body, _) => self.visit_while_stmt(condition, body),
            Stmt::ImportStmt(stmt) => self.visit_import_stmt(stmt),
            Stmt::BreakStmt(_) => self.visit_break_stmt(),
            Stmt::ContinueStmt(_) => self.visit_continue_stmt(),
            Stmt::ReturnStmt(return_expr, _) => self.visit_return_stmt(return_expr),
            Stmt::PrintStmt(expr, _) => self.visit_print_stmt(expr),
        }
    }

    fn visit_fun_decl(&mut self, fun: &mut FunctionDecl) -> VisitorResult {
        self.visit_name(&mut fun.id)?;

        for param in fun.params.iter_mut() {
            self.visit_ident(param)?;
        }

        self.visit_block_stmt(&mut fun.body)
    }

    fn visit_class_decl(&mut self, class: &mut ClassDecl) -> VisitorResult {
        self.visit_name(&mut class.id)?;

        for constructor in class.constructors.iter_mut() {
            self.visit_con_decl(constructor)?;
        }

        Ok(())
    }

    fn visit_con_decl(&mut self, con: &mut ConstructorDecl) -> VisitorResult {
        self.visit_name(&mut con.id)?;

        for param in con.params.iter_mut() {
            self.visit_ident(param)?;
        }

        self.visit_block_stmt(&mut con.body)
    }

    fn visit_var_decl(
        &mut self,
        stmt: &mut VarDeclaration,
    ) -> VisitorResult {
        self.visit_ident(&mut stmt.name)?;

        if let Some(init) = &mut stmt.init {
            self.visit_expr(init)?;
        }

        Ok(())
    }

    fn visit_block_stmt(&mut self, block: &mut Vec<Stmt>) -> VisitorResult {
        for stmt in block.iter_mut() {
            self.visit_stmt(stmt)?;
        }

        Ok(())
    }

    fn visit_if_stmt(
        &mut self,
        condition: &mut Expr,
        body: &mut Vec<Stmt>,
        alt: &mut Option<Box<Stmt>>,
    ) -> VisitorResult {
        self.visit_expr(condition)?;
        self.visit_block_stmt(body)?;

        if let Some(alt) = alt {
            self.visit_stmt(alt)?;
        }

        Ok(())
    }

    fn visit_loop_stmt(&mut self, body: &mut Vec<Stmt>) -> VisitorResult {
        self.visit_block_stmt(body)
    }

    fn visit_while_stmt(&mut self, condition: &mut Expr, body: &mut Vec<Stmt>) -> VisitorResult {
        self.visit_expr(condition)?;
        self.visit_block_stmt(body)
    }

    fn visit_import_stmt(&mut self, _import_stmt: &mut ImportStatement) -> VisitorResult {
        // Nothing to do (for now).
        Ok(())
    }

    fn visit_break_stmt(&mut self) -> VisitorResult {
        // Nothing to do.
        Ok(())
    }

    fn visit_continue_stmt(&mut self) -> VisitorResult {
        // Nothing to do.
        Ok(())
    }

    fn visit_return_stmt(&mut self, return_expr: &mut Option<Expr>) -> VisitorResult {
        if let Some(expr) = return_expr {
            self.visit_expr(expr)
        } else {
            Ok(())
        }
    }

    fn visit_print_stmt(&mut self, expr: &mut Expr) -> VisitorResult {
        self.visit_expr(expr)
    }

    fn visit_assignment(&mut self, stmt: &mut AssignmentStmt) -> VisitorResult {
        self.visit_expr(&mut stmt.lhs)?;
        self.visit_expr(&mut stmt.rhs)
    }

    fn visit_expr_stmt(&mut self, expr: &mut Expr) -> VisitorResult {
        self.visit_expr(expr)
    }

    fn visit_expr(&mut self, expr: &mut Expr) -> VisitorResult {
        match expr {
            Expr::ArrayExpr(array, _) => self.visit_array(array),
            Expr::MapExpr(values, _) => self.visit_map(values),
            Expr::BinaryExpr(expr, _) => self.visit_binary_expr(expr),
            Expr::ParenExpr(expr, _) => self.visit_paren_expr(expr),
            Expr::UnaryExpr(op, arg, _) => self.visit_unary_expr(op, arg),
            Expr::LogicalExpr(expr, _) => self.visit_logical_expr(expr),
            Expr::CallExpr(callee, args, _) => self.visit_call_expr(callee, args),
            Expr::MemberExpr(obj, prop, _) => self.visit_member_expr(obj, prop),
            Expr::Identifier(ident) => self.visit_ident(ident),
            Expr::Number(_, _) | Expr::Bool(_, _) | Expr::String(_, _) | Expr::Nil(_) => Ok(()),
        }
    }

    fn visit_array(&mut self, array: &mut [Expr]) -> VisitorResult {
        for element in array.iter_mut() {
            self.visit_expr(element)?;
        }

        Ok(())
    }

    fn visit_map(&mut self, values: &mut [Expr]) -> VisitorResult {
        for expr in values.iter_mut() {
            self.visit_expr(expr)?;
        }

        Ok(())
    }

    fn visit_binary_expr(&mut self, expr: &mut BinaryExpr) -> VisitorResult {
        self.visit_expr(&mut expr.lhs)?;
        self.visit_expr(&mut expr.rhs)
    }

    fn visit_logical_expr(&mut self, expr: &mut BinaryExpr) -> VisitorResult {
        self.visit_expr(&mut expr.lhs)?;
        self.visit_expr(&mut expr.rhs)
    }

    fn visit_paren_expr(&mut self, expr: &mut Expr) -> VisitorResult {
        self.visit_expr(expr)
    }

    fn visit_unary_expr(&mut self, _op: &mut Op, arg: &mut Expr) -> VisitorResult {
        self.visit_expr(arg)
    }

    fn visit_call_expr(&mut self, callee: &mut Expr, args: &mut Vec<Expr>) -> VisitorResult {
        self.visit_expr(callee)?;

        for arg in args.iter_mut() {
            self.visit_expr(arg)?;
        }

        Ok(())
    }

    fn visit_member_expr(&mut self, obj: &mut Expr, prop: &mut Expr) -> VisitorResult {
        self.visit_expr(obj)?;
        self.visit_expr(prop)
    }

    fn visit_ident(&mut self, _ident: &mut Ident) -> VisitorResult {
        // Nothing to do here.
        Ok(())
    }

    fn visit_name(&mut self, _ident: &mut Ident) -> VisitorResult {
        // Nothing to do here.
        Ok(())
    }
}
