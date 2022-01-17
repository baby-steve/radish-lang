use crate::{
    common::{
        chunk::Chunk,
        opcode::Opcode,
        value::Value,
    },
    compiler::{
        ast::*,
        visitor::Visitor,
    },
};

pub struct Local {
    pub name: String,
    pub depth: usize,
}

pub struct Compiler {
    pub chunk: Chunk,
    pub scope_depth: usize,
    pub local_count: usize,
    pub locals: Vec<Local>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            chunk: Chunk {
                code: vec![],
                constants: vec![],
            },
            scope_depth: 0,
            local_count: 0,
            locals: vec![],
        }
    }

    pub fn run(&mut self, ast: &AST) {
        for node in &ast.items {
            self.visit(node);
        }

        self.emit_return();
    }

    fn emit_byte(&mut self, byte: u8) {
        // should also emit the byte's location in source?
        self.chunk.code.push(byte);
    }

    fn emit_bytes(&mut self, byte_1: u8, byte_2: u8) {
        self.emit_byte(byte_1);
        self.emit_byte(byte_2);
    }

    fn emit_return(&mut self) {
        self.emit_byte(Opcode::Halt as u8);
    }

    /// add a constant to the chunk's constant array. Returns the 
    /// constant's index in the constant array as a u32.
    fn make_constant(&mut self, value: Value) -> u32 {
        self.chunk.add_constant(value) as u32
    }

    fn emit_constant(&mut self, value: Value) {
        let index = self.make_constant(value);

        if index > 255 {
            self.emit_byte(Opcode::LoadConstLong as u8);

            let bytes = index.to_le_bytes();
            println!("{:?}", bytes);

            for byte in bytes {
                self.emit_byte(byte);
            }

        } else {
            self.emit_bytes(Opcode::LoadConst as u8, index as u8);
        }
    }

    /// Add a string to the constants array. Returns the identifier's index
    /// in the constant array as a [`u32`].
    fn identifier_constant(&mut self, name: &str) -> u32 {
        // Todo: if the name is already in the constant table, return that.
        self.make_constant(Value::String(name.to_string()))
    }

    fn define_variable(&mut self, global: u32) {
        if self.scope_depth > 0 {
            return
        }

        self.emit_byte(Opcode::DefGlobal as u8);

        for byte in global.to_le_bytes() {
            self.emit_byte(byte);
        }
    }

    fn load_variable(&mut self, name: &str) {
        // try and resolve the name as local.
        let arg = match self.resolve_local(name) {
            // if the result is Some(_) then it must be local.
            Some(index) => {
                // emit a local get opcode and use the index returned from resolve_local.
                self.emit_byte(Opcode::GetLocal as u8);
                index as u32
            }
            // if the result is None, then it must be a global.
            None => {
                // emit a global get opcode and get its arg from identifier_constant.
                self.emit_byte(Opcode::GetGlobal as u8);
                self.identifier_constant(name)
            }
        };

        for byte in arg.to_le_bytes() {
            self.emit_byte(byte);
        }
    }

    fn save_variable(&mut self, name: &str) {
        let arg = match self.resolve_local(name) {
            Some(index) => {
                self.emit_byte(Opcode::SetLocal as u8);
                index as u32
            }
            None => {
                self.emit_byte(Opcode::SetGlobal as u8);
                self.identifier_constant(name)
            }
        };

        for byte in arg.to_le_bytes() {
            self.emit_byte(byte);
        }
    }

    fn enter_scope(&mut self) { 
        self.scope_depth += 1 
    }

    fn leave_scope(&mut self) { 
        self.scope_depth -= 1;

        while self.local_count > 0 && 
            self.locals[self.local_count - 1].depth > self.scope_depth
        {
            self.emit_byte(Opcode::Pop as u8);
            self.local_count -= 1;
        }
    }

    /// Add a [`Local`] to the [`Compiler`]'s locals array.
    fn add_local(&mut self, name: &str) {
        let local = Local { name: name.to_string(), depth: self.scope_depth, };
        self.locals.push(local);
        self.local_count += 1;
    }

    /// Resolve a local variable.
    fn resolve_local(&mut self, name: &str) -> Option<usize> {
        let mut index = self.local_count;
        while index > 0 {
            let local = &self.locals[index - 1];

            if local.name == name {
                return Some(index - 1);
            }

            index -= 1;
        }

        None
    }
}

impl Visitor for Compiler {
    fn block(&mut self, block: &BlockStmt) {
        self.enter_scope();

        for node in &block.body {
            self.visit(node);
        }

        self.leave_scope();
    }

    fn print(&mut self, expr: &ASTNode) {
        self.visit(&expr);

        self.emit_byte(Opcode::Print as u8);
    }

    fn var_declaration(&mut self, decl: &VarDeclaration) {
        if self.scope_depth > 0 {
            if let Some(expr) = &decl.init {
                self.visit(&expr);
            } else {
                self.emit_byte(Opcode::Nil as u8);
            }

            self.add_local(&decl.id.name);
        } else {
            let global = self.identifier_constant(&decl.id.name);

            if let Some(expr) = &decl.init {
                self.visit(&expr);
            } else {
                self.emit_byte(Opcode::Nil as u8);
            }

            self.define_variable(global);
        }
    }

    fn assignment(&mut self, stmt: &Assignment) {
        self.visit(&stmt.expr);

        self.save_variable(&stmt.id.name);
    }

    fn expression_stmt(&mut self, stmt: &ExpressionStmt) {
        self.visit(&stmt.expr);

        self.emit_byte(Opcode::Pop as u8);
    }

    fn binary_expression(&mut self, expr: &BinaryExpr) {
        self.visit(&expr.left);
        self.visit(&expr.right);

        match &expr.op {
            Op::Add => self.emit_byte(Opcode::Add as u8),
            Op::Subtract => self.emit_byte(Opcode::Subtract as u8),
            Op::Multiply => self.emit_byte(Opcode::Multiply as u8),
            Op::Divide => self.emit_byte(Opcode::Divide as u8),
            Op::LessThan => self.emit_byte(Opcode::LessThan as u8),
            Op::LessThanEquals => self.emit_byte(Opcode::LessThanEquals as u8),
            Op::GreaterThan => self.emit_byte(Opcode::GreaterThan as u8),
            Op::GreaterThanEquals => self.emit_byte(Opcode::GreaterThanEquals as u8),
            Op::EqualsTo => self.emit_byte(Opcode::EqualsTo as u8),      
            Op::NotEqual => self.emit_byte(Opcode::NotEqual as u8),      
            _ => unreachable!("{:?} is not a binary operator.", &expr.op),
        }
    }

    fn unary(&mut self, node: &UnaryExpr) {
        self.visit(&node.arg);

        match &node.op {
            Op::Subtract => self.emit_byte(Opcode::Negate as u8),
            Op::Bang => self.emit_byte(Opcode::Not as u8),
            _ => unreachable!("{:?} is not an unary operator.", &node.op),
        }
    }

    fn identifier(&mut self, id: &Ident) {
        self.load_variable(&id.name);
    }

    fn number(&mut self, val: &f64) {
        self.emit_constant(Value::Number(*val));
    }

    fn string(&mut self, val: &str) {
        self.emit_constant(Value::String(val.to_string()));
    }

    fn boolean(&mut self, val: &bool) {
        match val {
            true => self.emit_byte(Opcode::True as u8),
            false => self.emit_byte(Opcode::False as u8),
        }
    }

    fn nil(&mut self) {
        self.emit_byte(Opcode::Nil as u8);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::rc::Rc;

    use crate::compiler::parser::Parser;
    use crate::common::source::Source;

    fn run_test_compiler(test_string: &str) -> Compiler {
        let source = Source::source(test_string);
        let result = Parser::new(Rc::clone(&source)).parse().unwrap();
        let mut compiler = Compiler::new();
        compiler.run(&result);
        compiler
    }

    #[test]
    fn compile_binary_add_expr() {
        let result = run_test_compiler("1 + 23");
        assert_eq!(
            result.chunk.code,
            vec!(
                Opcode::LoadConst as u8, 0,
                Opcode::LoadConst as u8, 1,
                Opcode::Add as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
            result.chunk.constants,
            vec!(Value::Number(1.0), Value::Number(23.0),)
        );
    }

    #[test]
    fn compile_binary_sub_expr() {
        let result = run_test_compiler("12 - 6");
        assert_eq!(
         result.chunk.code,
            vec!(
                Opcode::LoadConst as u8, 0,
                Opcode::LoadConst as u8, 1,
                Opcode::Subtract as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
         result.chunk.constants,
            vec!(Value::Number(12.0), Value::Number(6.0),)
        );
    }

    #[test]
    fn compile_binary_mul_expr() {
        let result = run_test_compiler("5 * 2");
        assert_eq!(
         result.chunk.code,
            vec!(
                Opcode::LoadConst as u8, 0,
                Opcode::LoadConst as u8, 1,
                Opcode::Multiply as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
         result.chunk.constants,
            vec!(Value::Number(5.0), Value::Number(2.0),)
        );
    }

    #[test]
    fn compile_binary_div_expr() {
        let result = run_test_compiler("12 / 4");
        assert_eq!(
         result.chunk.code,
            vec!(
                Opcode::LoadConst as u8, 0,
                Opcode::LoadConst as u8, 1,
                Opcode::Divide as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
         result.chunk.constants,
            vec!(Value::Number(12.0), Value::Number(4.0),)
        );
    }

    #[test]
    fn compile_less_than_expr() {
        let result = run_test_compiler("5 < 2");
        assert_eq!(
         result.chunk.code,
            vec!(
                Opcode::LoadConst as u8, 0,
                Opcode::LoadConst as u8, 1,
                Opcode::LessThan as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
         result.chunk.constants,
            vec!(Value::Number(5.0), Value::Number(2.0))
        );
    }

    #[test]
    fn compile_less_than_equals_expr() {
        let result = run_test_compiler("5 <= 2");
        assert_eq!(
         result.chunk.code,
            vec!(
                Opcode::LoadConst as u8, 0,
                Opcode::LoadConst as u8, 1,
                Opcode::LessThanEquals as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
         result.chunk.constants,
            vec!(Value::Number(5.0), Value::Number(2.0))
        );
    }

    #[test]
    fn compile_greater_than_expr() {
        let result = run_test_compiler("5 > 2");
        assert_eq!(
         result.chunk.code,
            vec!(
                Opcode::LoadConst as u8, 0,
                Opcode::LoadConst as u8, 1,
                Opcode::GreaterThan as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
         result.chunk.constants,
            vec!(Value::Number(5.0), Value::Number(2.0),)
        );
    }

    #[test]
    fn compile_greater_than_equal_expr() {
        let result = run_test_compiler("5 >= 2");
        assert_eq!(
         result.chunk.code,
            vec!(
                Opcode::LoadConst as u8, 0,
                Opcode::LoadConst as u8, 1,
                Opcode::GreaterThanEquals as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
         result.chunk.constants,
            vec!(Value::Number(5.0), Value::Number(2.0),)
        );
    }

    #[test]
    fn compile_equals_to_expr() {
        let result = run_test_compiler("5 == 2");
        assert_eq!(
         result.chunk.code,
            vec!(
                Opcode::LoadConst as u8, 0,
                Opcode::LoadConst as u8, 1,
                Opcode::EqualsTo as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
         result.chunk.constants,
            vec!(Value::Number(5.0), Value::Number(2.0),)
        );
    }

    #[test]
    fn compile_not_equal_expr() {
        let result = run_test_compiler("5 != 2");
        assert_eq!(
         result.chunk.code,
            vec!(
                Opcode::LoadConst as u8, 0,
                Opcode::LoadConst as u8, 1,
                Opcode::NotEqual as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
         result.chunk.constants,
            vec!(Value::Number(5.0), Value::Number(2.0))
        );
    }

    #[test]
    fn compile_multiple_binary_expr() {
        let result = run_test_compiler("1 + 23 * 5");
        assert_eq!(
         result.chunk.code,
            vec!(
                Opcode::LoadConst as u8, 0,
                Opcode::LoadConst as u8, 1,
                Opcode::LoadConst as u8, 2,
                Opcode::Multiply as u8,
                Opcode::Add as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
         result.chunk.constants,
            vec!(Value::Number(1.0), Value::Number(23.0), Value::Number(5.0),)
        )
    }

    #[test]
    fn compile_unary_expr() {
        let result = run_test_compiler("-23");
        assert_eq!(
         result.chunk.code,
            vec!(
                Opcode::LoadConst as u8, 0,
                Opcode::Negate as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(result.chunk.constants, vec!(Value::Number(23.0)))
    }

    #[test]
    fn compile_unary_not() {
        let result = run_test_compiler("!true");
        assert_eq!(
            result.chunk.code,
            vec![
                Opcode::True as u8,
                Opcode::Not as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            ]
        );
    }

    #[test]
    fn compile_boolean_literal() {
        let result = run_test_compiler("true");
        assert_eq!(result.chunk.code, vec!(Opcode::True as u8, Opcode::Pop as u8, Opcode::Halt as u8));

        let result = run_test_compiler("false");
        assert_eq!(result.chunk.code, vec!(Opcode::False as u8, Opcode::Pop as u8, Opcode::Halt as u8));
    }

    #[test]
    fn compile_nil_literal() {
        let result = run_test_compiler("nil");
        assert_eq!(result.chunk.code, vec![Opcode::Nil as u8, Opcode::Pop as u8, Opcode::Halt as u8]);
    }

    #[test]
    fn compile_variable_declaration() {
        let result = run_test_compiler("var a");
        assert_eq!(
            result.chunk.code,
            vec![
                Opcode::Nil as u8,
                Opcode::DefGlobal as u8, 0, 0, 0, 0,
                Opcode::Halt as u8,
            ]
        )
    }

    #[test]
    fn compile_variable_declaration_with_value() {
        let result = run_test_compiler("var a = 23");
        assert_eq!(
            result.chunk.code,
            vec![
                Opcode::LoadConst as u8, 1,
                Opcode::DefGlobal as u8, 0, 0, 0, 0,
                Opcode::Halt as u8,
            ]
        )
    }

    #[test]
    fn compile_print_statement() {
        let result = run_test_compiler("print 23");
        assert_eq!(
            result.chunk.code,
            vec![
                Opcode::LoadConst as u8, 0,
                Opcode::Print as u8,
                Opcode::Halt as u8,
            ]
        )
    }

    #[test]
    fn compile_string_literal() {
        let result = run_test_compiler("\"Hello, World!\"");
        assert_eq!(
            result.chunk.code,
            vec![
                Opcode::LoadConst as u8, 0,
                Opcode::Pop as u8,
                Opcode::Halt as u8, 
            ]
        );
    }
}
