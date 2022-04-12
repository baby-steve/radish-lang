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
            Stmt::VarDeclaration(id, expr, kind, _) => self.visit_var_decl(id, expr, *kind),
            Stmt::Assignment(id, op, expr, _) => self.visit_assignment(id, op, expr),
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
        id: &mut Ident,
        expr: &mut Option<Expr>,
        _kind: VarKind,
    ) -> VisitorResult {
        self.visit_ident(id)?;

        if let Some(init) = expr {
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

    fn visit_assignment(
        &mut self,
        id: &mut Ident,
        _op: &mut OpAssignment,
        expr: &mut Expr,
    ) -> VisitorResult {
        self.visit_ident(id)?;
        self.visit_expr(expr)
    }

    fn visit_expr_stmt(&mut self, expr: &mut Expr) -> VisitorResult {
        self.visit_expr(expr)
    }

    fn visit_expr(&mut self, expr: &mut Expr) -> VisitorResult {
        match expr {
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
/*
fn walk_fun_decl<'a, V: Visitor<'a>>(visitor: &mut V, fun: &mut FunctionDecl) -> VisitorResult {
    Ok(())
}

fn walk_class_decl(visitor: &mut Visitor, class: &mut ClassDecl) -> VisitorResult {
    Ok(())
}

fn walk_con_decl(visitor: &mut Visitor, con: &mut ConstructorDecl) -> VisitorResult {
    Ok(())
}

fn walk_var_decl(visitor: &mut Visitor) -> VisitorResult {
    Ok(())
}

fn walk_block_stmt(visitor: &mut Visitor) -> VisitorResult {
    Ok(())
}

fn walk_if_stmt(visitor: &mut Visitor) -> VisitorResult {
    Ok(())
}

fn walk_loop_stmt(visitor: &mut Visitor) -> VisitorResult {
    Ok(())
}

fn walk_while_stmt(visitor: &mut Visitor) -> VisitorResult {
    Ok(())
}

fn walk_break_stmt(visitor: &mut Visitor) -> VisitorResult {
    Ok(())
}

fn walk_continue_stmt(visitor: &mut Visitor) -> VisitorResult {
    Ok(())
}

fn walk_return_stmt(visitor: &mut Visitor) -> VisitorResult {
    Ok(())
}

fn walk_print_stmt(visitor: &mut Visitor) -> VisitorResult {
    Ok(())
}

fn walk_binary_expr(visitor: &mut Visitor) -> VisitorResult {
    Ok(())
}

fn walk_paren_expr(visitor: &mut Visitor) -> VisitorResult {
    Ok(())
}

fn walk_unary_expr(visitor: &mut Visitor) -> VisitorResult {
    Ok(())
}

fn walk_logical_expr(visitor: &mut Visitor) -> VisitorResult {
    Ok(())
}

fn walk_call_expr(visitor: &mut Visitor) -> VisitorResult {
    Ok(())
}

fn walk_member_expr(visitor: &mut Visitor) -> VisitorResult {
    Ok(())
}

fn walk_assignment(visitor: &mut Visitor) -> VisitorResult {
    Ok(())
}

fn walk_expr_stmt(visitor: &mut Visitor) -> VisitorResult {
    Ok(())
}*/
