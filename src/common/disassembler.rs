use crate::common::chunk::Chunk;

pub struct Disassembler<'a> {
    name: &'a str,
    chunk: &'a Chunk,
}

impl<'a> Disassembler<'a> {
    pub fn new(name: &'a str, chunk: &'a Chunk) -> Self {
        Disassembler { name, chunk }
    }

    pub fn disassemble_chunk(name: &str, chunk: &Chunk) { 
        let dis = Disassembler::new(name, chunk);

        println!("Disassembling \"{}\"...", dis.name);       
        let mut offset = 0;

        println!("==== Code ====================");
        while offset < dis.chunk.code.len() {
            offset = dis.disassemble_instruction(offset);
        }

        println!("==== Constants ===============");
        for con in &dis.chunk.constants {
            print!("[ {} ]", con);
        }

        print!("\n\n");
    }

    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        use crate::common::opcode::Opcode;

        let byte = self.chunk.code[offset];
        match Opcode::from(byte) {
            Opcode::LoadConst => self.byte_instruction("LoadConst", offset),
            Opcode::LoadConstLong => self.long_const_instruction("LoadConstLong", offset, true),
            Opcode::Pop => self.simple_instruction("Pop", offset),

            Opcode::DefGlobal => self.long_const_instruction("DefGlobal", offset, true),
            Opcode::GetGlobal => self.long_const_instruction("GetGlobal", offset, true),
            Opcode::SetGlobal => self.long_const_instruction("SetGlobal", offset, true),

            Opcode::GetLocal => self.long_const_instruction("GetLocal", offset, false),
            Opcode::SetLocal => self.long_const_instruction("SetLocal", offset, false),

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

            Opcode::JumpIfTrue => self.jump_instruction("JumpIfTrue", 1, offset),
            Opcode::JumpIfFalse => self.jump_instruction("JumpIfFalse", 1, offset),
            Opcode::Jump => self.jump_instruction("Jump", 1, offset),
            Opcode::Loop => self.jump_instruction("Loop", -1, offset),

            Opcode::Call => self.byte_instruction("Call", offset),

            Opcode::Print => self.simple_instruction("Print", offset),
            Opcode::Return => self.simple_instruction("Return", offset),
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
        print!("{}{}", index, i_padding);

        self.write_value(*index as usize);

        offset + 2
    } 

    fn long_const_instruction(&self, name: &str, offset: usize, has_value: bool) -> usize {
        use std::convert::TryInto;
        self.write_instruction(name, offset);

        let bytes = self.chunk.code[offset + 1..offset + 5]
            .try_into()
            .expect(&format!("Expected a slice of length {}.", 4));
        
        let index = u32::from_le_bytes(bytes);
        let i_padding = " ".repeat(self.chunk.constants.len().to_string().len() - index.to_string().len());
        print!("{}{}", index, i_padding);

        if has_value {
            self.write_value(index as usize);
        } else {
            print!("\n");
        }

        offset + 5
    }

    fn jump_instruction(&self, name: &str, sign: i8, offset: usize) -> usize {
        self.write_instruction(name, offset);

        let byte1 = self.chunk.code[offset + 1];
        let byte2 = self.chunk.code[offset + 2];
        
        let jump = u16::from_le_bytes([byte1, byte2]);
        let i_padding = " ".repeat(self.chunk.code.len().to_string().len() - jump.to_string().len());

        print!("{}{} -> {}\n", i_padding, offset, offset as i32 + 3 as i32 + sign as i32 * jump as i32);

        offset + 3
    }

    fn write_instruction(&self, name: &str, offset: usize) {
        let padding = " ".repeat(self.chunk.code.len().to_string().len() - offset.to_string().len());
        print!("{}{}: {:<20}", padding, offset, name);
    } 

    fn write_value(&self, index: usize) {
        println!(" ({})", self.chunk.constants[index]);
    }
}
