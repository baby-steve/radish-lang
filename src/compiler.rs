use crate::ast::{ASTNode, AST};
use crate::opcode::{Opcode};

pub struct Compiler {

    has_error: bool,
    panic_mode: bool,
    error_count: usize, // Make smaller?
}

impl Compiler {
    pub fn run(&self, ast: &AST) {
        
    }

    fn emitByte(&self, byte: u8) {}

    fn emitBytes(&self, byte_1: u8, byte_2: u8) {
        self.emitByte(byte_1);
        self.emitByte(byte_1);
    }

    fn emitReturn(&self) {
        self.emitByte(Opcode::Halt as u8);
    }

    fn expression(&self) {}

    fn number(&self) {}

    fn synchronize() {}
}
