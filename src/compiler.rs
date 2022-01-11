use crate::ast::*;
use crate::opcode::Opcode;
use crate::value::Value;
use crate::vm::Chunk;
use crate::visitor::Visitor;

pub struct Compiler {
    pub chunk: Chunk,
}

impl Visitor for Compiler {
    fn var_declaration(&mut self, decl: &VarDeclaration) {
        let global = self.identifier_constant(&decl.id.name);

        if let Some(expr) = &decl.init {
            self.visit(&expr);
        } else {
            self.emit_byte(Opcode::Nil as u8);
        }

        self.define_variable(global);
    }

    fn assignment(&mut self, stmt: &Assignment) {
        self.visit(&stmt.expr);

        let arg = self.identifier_constant(&stmt.id.name);
        self.emit_bytes(Opcode::SetGlobal as u8, arg);
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
        self.named_variable(&id.name);
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

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            chunk: Chunk {
                code: vec![],
                constants: vec![],
            },
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

    fn emit_constant(&mut self, value: Value) {
        // Todo: if there are over 255 constants in one chunk, should emit a load_long opcode.
        let index = self.make_constant(value);
        self.emit_bytes(Opcode::LoadConst as u8, index);
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        self.chunk.add_constant(value) as u8
    }

    fn identifier_constant(&mut self, name: &str) -> u8 {
        self.make_constant(Value::String(name.to_string())) as u8
    }

    fn define_variable(&mut self, global: u8) {
        self.emit_bytes(Opcode::DefGlobal as u8, global);
    }

    fn named_variable(&mut self, name: &str) {
        let arg = self.identifier_constant(name);
        self.emit_bytes(Opcode::GetGlobal as u8, arg);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::rc::Rc;

    use crate::parser::Parser;
    use crate::source::Source;

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
                Opcode::DefGlobal as u8, 0,
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
                Opcode::DefGlobal as u8, 0,
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
