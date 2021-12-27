use crate::ast::*;
use crate::opcode::Opcode;
use crate::value::Value;
use crate::vm::Chunk;

pub struct Compiler {
    pub chunk: Chunk,
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

    fn visit(&mut self, node: &ASTNode) {
        match node {
            ASTNode::BinaryExpr(expr, _) => self.expression(expr),
            ASTNode::ParenExpr(expr, _) => self.grouping(expr),
            ASTNode::UnaryExpr(arg, _) => self.unary(arg),
            ASTNode::Literal(lit, _) => self.literal(lit),
        }
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
        self.emit_bytes(Opcode::Constant as u8, index);
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        self.chunk.add_constant(value) as u8
    }

    fn grouping(&mut self, expr: &ParenExpr) {
        self.visit(&expr.expr);
    }

    fn expression(&mut self, expr: &BinaryExpr) {
        self.visit(&expr.left);
        self.visit(&expr.right);

        match expr.op {
            Op::Add => self.emit_byte(Opcode::Add as u8),
            Op::Subtract => self.emit_byte(Opcode::Subtract as u8),
            Op::Multiply => self.emit_byte(Opcode::Multiply as u8),
            Op::Divide => self.emit_byte(Opcode::Divide as u8),
        }
    }

    fn unary(&mut self, node: &UnaryExpr) {
        self.visit(&node.arg);

        match &node.op {
            Op::Subtract => self.emit_byte(Opcode::Negate as u8),
            _ => unreachable!(), // Add error message?
        }
    }

    fn literal(&mut self, node: &Literal) {
        match node {
            Literal::Number(val) => self.number(val),
            Literal::Bool(val) => self.boolean(val),
        }
    }

    fn number(&mut self, val: &f64) {
        self.emit_constant(Value::Number(*val));
    }

    fn boolean(&mut self, val: &bool) {
        match val {
            true => self.emit_byte(Opcode::True as u8),
            false => self.emit_byte(Opcode::False as u8),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    #[test]
    fn compile_binary_add_expr() {
        let result = Parser::new("1 + 23").parse().unwrap();
        let mut compiler = Compiler::new();
        compiler.run(&result);
        assert_eq!(
            compiler.chunk.code,
            vec!(
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::Add as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
            compiler.chunk.constants,
            vec!(Value::Number(1.0), Value::Number(23.0),)
        );
    }

    #[test]
    fn compile_binary_sub_expr() {
        let result = Parser::new("1 - 23").parse().unwrap();
        let mut compiler = Compiler::new();
        compiler.run(&result);
        assert_eq!(
            compiler.chunk.code,
            vec!(
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::Subtract as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
            compiler.chunk.constants,
            vec!(Value::Number(1.0), Value::Number(23.0),)
        );
    }

    #[test]
    fn compile_binary_mul_expr() {
        let result = Parser::new("1 * 23").parse().unwrap();
        let mut compiler = Compiler::new();
        compiler.run(&result);
        assert_eq!(
            compiler.chunk.code,
            vec!(
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::Multiply as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
            compiler.chunk.constants,
            vec!(Value::Number(1.0), Value::Number(23.0),)
        );
    }

    #[test]
    fn compile_binary_div_expr() {
        let result = Parser::new("1 / 23").parse().unwrap();
        let mut compiler = Compiler::new();
        compiler.run(&result);
        assert_eq!(
            compiler.chunk.code,
            vec!(
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::Divide as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
            compiler.chunk.constants,
            vec!(Value::Number(1.0), Value::Number(23.0),)
        );
    }

    #[test]
    fn compile_multiple_binary_expr() {
        let result = Parser::new("1 + 23 * 5").parse().unwrap();
        let mut compiler = Compiler::new();
        compiler.run(&result);
        assert_eq!(
            compiler.chunk.code,
            vec!(
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::Constant as u8, 2,
                Opcode::Multiply as u8,
                Opcode::Add as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
            compiler.chunk.constants,
            vec!(Value::Number(1.0), Value::Number(23.0), Value::Number(5.0),)
        )
    }

    #[test]
    fn compile_unary_expr() {
        let result = Parser::new("-23").parse().unwrap();
        let mut compiler = Compiler::new();
        compiler.run(&result);
        assert_eq!(
            compiler.chunk.code,
            vec!(
                Opcode::Constant as u8, 0,
                Opcode::Negate as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(compiler.chunk.constants, vec!(Value::Number(23.0)))
    }

    #[test]
    fn compile_boolean_literal() {
        let result = Parser::new("true").parse().unwrap();
        let mut compiler = Compiler::new();
        compiler.run(&result);
        assert_eq!(compiler.chunk.code, vec!(Opcode::True as u8, Opcode::Halt as u8));

        let result = Parser::new("true").parse().unwrap();
        let mut compiler = Compiler::new();
        compiler.run(&result);
        assert_eq!(compiler.chunk.code, vec!(Opcode::True as u8, Opcode::Halt as u8));
    }
}
