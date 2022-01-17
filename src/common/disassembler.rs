use crate::common::chunk::Chunk;

pub struct Disassembler<'a> {
    name: &'a str,
    chunk: &'a Chunk,
}

impl<'a> Disassembler<'a> {
    pub fn new(name: &'a str, chunk: &'a Chunk) -> Self {
        Disassembler { name, chunk }
    }

    pub fn disassemble(&self) { 
        println!("Disassembling {}...", self.name);       
        let mut offset = 0;

        println!("==== Code ====");
        while offset < self.chunk.code.len() {
            offset = self.disassemble_instruction(offset);
        }

        println!("==== Constants ====");
        for con in &self.chunk.constants {
            print!("[ {} ]", con);
        }

        print!("\n");
    }

    fn disassemble_instruction(&self, offset: usize) -> usize {
        use crate::common::opcode::Opcode;

        let byte = self.chunk.code[offset];
        match Opcode::from(byte) {
            Opcode::LoadConst => self.byte_instruction("LoadConst", offset),
            Opcode::LoadConstLong => self.long_const_instruction("LoadConstLong", offset),
            Opcode::Pop => self.simple_instruction("Pop", offset),

            Opcode::DefGlobal => self.long_const_instruction("DefGlobal", offset),
            Opcode::GetGlobal => self.long_const_instruction("GetGlobal", offset),
            Opcode::SetGlobal => self.long_const_instruction("SetGlobal", offset),

            Opcode::GetLocal => self.long_const_instruction("GetLocal", offset),
            Opcode::SetLocal => self.long_const_instruction("SetLocal", offset),

            Opcode::True => self.simple_instruction("True", offset),
            Opcode::False => self.simple_instruction("False", offset),
            Opcode::Nil => self.simple_instruction("Nil", offset),

            Opcode::Add => self.simple_instruction("Add", offset),
            Opcode::Subtract => self.simple_instruction("Subtract", offset),
            Opcode::Multiply => self.simple_instruction("Multiply", offset),
            Opcode::Divide => self.simple_instruction("Divide", offset),
            Opcode::LessThan => self.simple_instruction("LessThan", offset),
            Opcode::LessThanEquals => self.simple_instruction("LessThanEquals", offset),
            Opcode::GreaterThan => self.simple_instruction("GreaterThan", offset),
            Opcode::GreaterThanEquals => self.simple_instruction("GreaterThanEquals", offset),
            Opcode::EqualsTo => self.simple_instruction("EqualsTo", offset),
            Opcode::NotEqual => self.simple_instruction("NotEqual", offset),
            Opcode::Negate => self.simple_instruction("Negate", offset),
            Opcode::Not => self.simple_instruction("Not", offset),
            Opcode::Print => self.simple_instruction("Print", offset),
            Opcode::Halt => self.simple_instruction("Halt", offset),
        }
    }

    fn simple_instruction(&self, name: &str, offset: usize) -> usize {
        self.write_instruction(name, offset);
        print!("\n");
        offset + 1
    }

    fn byte_instruction(&self, name: &str, offset: usize) -> usize {
        self.write_instruction(name, offset);
        
        let index = &self.chunk.code[offset + 1];
        let i_padding = " ".repeat(self.chunk.constants.len().to_string().len() - index.to_string().len());
        print!("{}{}", i_padding, index);

        self.write_value(*index as usize);

        offset + 2
    } 

    fn long_const_instruction(&self, name: &str, offset: usize) -> usize {
        use std::convert::TryInto;
        self.write_instruction(name, offset);

        let bytes = self.chunk.code[offset + 1..offset + 5]
            .try_into()
            .expect(&format!("Expected a slice of length {}.", 4));
        
        let index = u32::from_le_bytes(bytes);
        let i_padding = " ".repeat(self.chunk.constants.len().to_string().len() - index.to_string().len());
        print!("{}{}", i_padding, index);

        self.write_value(index as usize);

        offset + 5
    }

    fn write_instruction(&self, name: &str, offset: usize) {
        let padding = " ".repeat(self.chunk.code.len().to_string().len() - offset.to_string().len());
        print!("{}{}: {:<14}", padding, offset, name);
    } 

    fn write_value(&self, index: usize) {
        println!(" ; {}", self.chunk.constants[index]);
    }
}
