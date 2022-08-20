//! Module containing an AST visitor.

use crate::compiler::ast::*;

use super::{
    ast_ast::{
        ArrayExpr, BinaryExpr, BlockStmt, BreakStmt, CallExpr, ContinueStmt, FunctionDecl,
        Identifier, IfStmt, ImportStmt, LogicalExpr, LoopStmt, MapExpr, MemberExpr, ReturnStmt,
        UnaryExpr, WhileStmt, ExpressionStmt, VariableDecl, AssignmentExpr, MethodDecl, ClassDecl,
    },
    SyntaxError,
};

/// Alias for the Visitor method's return type.
pub type VisitorResult = Result<(), SyntaxError>;

/// Transverses an AST's nodes. By default, it recursively walks the
/// tree and does absolutely nothing.
pub trait Visitor<'a>: Sized {
    fn visit_stmt(&mut self, stmt: &mut Stmt) -> VisitorResult {
        match stmt {
            Stmt::BlockStmt(block_stmt) => self.visit_block_stmt(block_stmt),
            Stmt::ExpressionStmt(expr) => self.visit_expr_stmt(expr),
            Stmt::VariableDecl(stmt) => self.visit_var_decl(stmt),
            Stmt::FunctionDecl(fun) => self.visit_fun_decl(fun),
            Stmt::MethodDecl(method) => self.visit_method_decl(method),
            Stmt::ClassDecl(class) => self.visit_class_decl(class),
            Stmt::IfStmt(stmt) => self.visit_if_stmt(stmt),
            Stmt::LoopStmt(stmt) => self.visit_loop_stmt(stmt),
            Stmt::WhileStmt(stmt) => self.visit_while_stmt(stmt),
            Stmt::BreakStmt(stmt) => self.visit_break_stmt(stmt),
            Stmt::ContinueStmt(stmt) => self.visit_continue_stmt(stmt),
            Stmt::ReturnStmt(return_expr) => self.visit_return_stmt(return_expr),
            Stmt::ImportStmt(stmt) => self.visit_import_stmt(stmt),
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

    fn visit_method_decl(&mut self, method: &mut MethodDecl) -> VisitorResult {
        self.visit_fun_decl(&mut method.body)
    }

    fn visit_class_decl(&mut self, class: &mut ClassDecl) -> VisitorResult {
        self.visit_name(&mut class.id)?;

        // for constructor in class.constructors.iter_mut() {
        //     self.visit_con_decl(constructor)?;
        // }

        Ok(())
    }

    fn visit_var_decl(&mut self, stmt: &mut VariableDecl) -> VisitorResult {
        self.visit_ident(&mut stmt.id)?;

        if let Some(init) = &mut stmt.init {
            self.visit_expr(init)?;
        }

        Ok(())
    }

    fn visit_block_stmt(&mut self, block_stmt: &mut BlockStmt) -> VisitorResult {
        for stmt in block_stmt.body.iter_mut() {
            self.visit_stmt(stmt)?;
        }

        Ok(())
    }

    fn visit_if_stmt(&mut self, stmt: &mut IfStmt) -> VisitorResult {
        self.visit_expr(&mut stmt.condition)?;

        self.visit_block_stmt(&mut stmt.consequent)?;

        if let Some(alt) = &mut stmt.alternate {
            self.visit_stmt(alt)?;
        }

        Ok(())
    }

    fn visit_loop_stmt(&mut self, stmt: &mut LoopStmt) -> VisitorResult {
        self.visit_block_stmt(&mut stmt.body)
    }

    fn visit_while_stmt(&mut self, stmt: &mut WhileStmt) -> VisitorResult {
        self.visit_expr(&mut stmt.condition)?;
        self.visit_block_stmt(&mut stmt.body)
    }

    fn visit_import_stmt(&mut self, _import_stmt: &mut ImportStmt) -> VisitorResult {
        // Nothing to do (for now).
        Ok(())
    }

    fn visit_break_stmt(&mut self, _: &mut BreakStmt) -> VisitorResult {
        // Nothing to do.
        Ok(())
    }

    fn visit_continue_stmt(&mut self, _: &mut ContinueStmt) -> VisitorResult {
        // Nothing to do.
        Ok(())
    }

    fn visit_return_stmt(&mut self, return_stmt: &mut ReturnStmt) -> VisitorResult {
        if let Some(expr) = &mut return_stmt.argument {
            self.visit_expr(expr)
        } else {
            Ok(())
        }
    }

    fn visit_print_stmt(&mut self, expr: &mut Expr) -> VisitorResult {
        self.visit_expr(expr)
    }

    fn visit_assignment(&mut self, expr: &mut AssignmentExpr) -> VisitorResult {
        self.visit_expr(&mut expr.lhs)?;
        self.visit_expr(&mut expr.rhs)
    }

    fn visit_expr_stmt(&mut self, stmt: &mut ExpressionStmt) -> VisitorResult {
        self.visit_expr(&mut stmt.expr)
    }

    fn visit_expr(&mut self, expr: &mut Expr) -> VisitorResult {
        match expr {
            Expr::ArrayExpr(array) => self.visit_array(array),
            Expr::MapExpr(map) => self.visit_map(map),
            Expr::AssignmentExpr(expr) => self.visit_assignment(expr),
            Expr::BinaryExpr(expr) => self.visit_binary_expr(expr),
            Expr::UnaryExpr(expr) => self.visit_unary_expr(expr),
            Expr::LogicalExpr(expr) => self.visit_logical_expr(expr),
            Expr::CallExpr(expr) => self.visit_call_expr(expr),
            Expr::MemberExpr(expr) => self.visit_member_expr(expr),
            Expr::This(_) => self.visit_this(),
            Expr::Identifier(ident) => self.visit_ident(ident),
            Expr::Literal(_) => Ok(()),
        }
    }

    fn visit_array(&mut self, array: &mut ArrayExpr) -> VisitorResult {
        for element in array.elements.iter_mut() {
            self.visit_expr(element)?;
        }

        Ok(())
    }

    fn visit_map(&mut self, map: &mut MapExpr) -> VisitorResult {
        for prop in map.properties.iter_mut() {
            self.visit_expr(&mut prop.key)?;
            self.visit_expr(&mut prop.value)?;
        }

        Ok(())
    }

    fn visit_binary_expr(&mut self, expr: &mut BinaryExpr) -> VisitorResult {
        self.visit_expr(&mut expr.lhs)?;
        self.visit_expr(&mut expr.rhs)
    }

    fn visit_logical_expr(&mut self, expr: &mut LogicalExpr) -> VisitorResult {
        self.visit_expr(&mut expr.lhs)?;
        self.visit_expr(&mut expr.rhs)
    }

    fn visit_unary_expr(&mut self, expr: &mut UnaryExpr) -> VisitorResult {
        self.visit_expr(&mut expr.argument)
    }

    fn visit_call_expr(&mut self, expr: &mut CallExpr) -> VisitorResult {
        self.visit_expr(&mut expr.callee)?;

        for arg in expr.args.iter_mut() {
            self.visit_expr(arg)?;
        }

        Ok(())
    }

    fn visit_member_expr(&mut self, expr: &mut MemberExpr) -> VisitorResult {
        self.visit_expr(&mut expr.object)?;
        self.visit_expr(&mut expr.property)
    }

    fn visit_ident(&mut self, _ident: &mut Identifier) -> VisitorResult {
        // Nothing to do here.
        Ok(())
    }

    fn visit_this(&mut self) -> VisitorResult {
        // Nothing to do here.
        Ok(())
    }

    fn visit_name(&mut self, _ident: &mut Identifier) -> VisitorResult {
        // Nothing to do here.
        Ok(())
    }
}
