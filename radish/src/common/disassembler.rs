// TODO: rewrite this entire thing as it's gotten quite gnarly and is due for a refactor.

use crate::vm::value::Function;

pub struct Disassembler<'a> {
    name: &'a str,
    function: &'a Function,
}

impl<'a> Disassembler<'a> {
    pub fn new(name: &'a str, function: &'a Function) -> Self {
        Disassembler { name, function }
    }

    pub fn disassemble_chunk(name: &str, function: &Function) {
        let dis = Disassembler::new(name, function);

        println!("Disassembling \"{}\"...", dis.name);
        let mut offset = 0;
        
        println!("==== Constants ===============");
        for con in &dis.function.chunk.constants {
            print!("[ {} ]", con);
        }

        print!("\n\n");

        println!("==== Code ====================");
        while offset < dis.function.chunk.code.len() {
            offset = dis.disassemble_instruction(offset);
        }

        print!("\n\n");
    }

    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        use crate::common::opcode::Opcode;

        let byte = self.function.chunk.code[offset];
        match Opcode::from(byte) {
            Opcode::LoadConst => self.byte_instruction("LoadConst", offset),
            Opcode::LoadConstLong => self.long_const_instruction("LoadConstLong", offset, true),

            Opcode::Del => self.simple_instruction("Pop", offset),

            Opcode::DefGlobal => self.write_global("DefGlobal", offset),
            Opcode::LoadGlobal => self.write_global("GetGlobal", offset),
            Opcode::SaveGlobal => self.write_global("SetGlobal", offset),

            Opcode::LoadLocal => self.long_const_instruction("GetLocal", offset, false),
            Opcode::SaveLocal => self.long_const_instruction("SetLocal", offset, false),

            Opcode::DefCapture => self.simple_instruction("DefCapture", offset),
            Opcode::LoadCapture => self.upvalue_instruction("LoadCapture", offset),
            Opcode::SaveCapture => self.upvalue_instruction("SaveCapture", offset),

            Opcode::LoadField => self.simple_instruction("LoadField", offset),
            Opcode::SaveField => self.long_const_instruction("SaveField", offset, false),

            Opcode::True => self.simple_instruction("True", offset),
            Opcode::False => self.simple_instruction("False", offset),
            Opcode::Nil => self.simple_instruction("Nil", offset),

            Opcode::Add => self.simple_instruction("Add", offset),
            Opcode::Sub => self.simple_instruction("Sub", offset),
            Opcode::Mul => self.simple_instruction("Mul", offset),
            Opcode::Div => self.simple_instruction("Div", offset),
            Opcode::Rem => self.simple_instruction("Rem", offset),

            Opcode::Neg => self.simple_instruction("Negate", offset),
            Opcode::Not => self.simple_instruction("Not", offset),
            Opcode::CmpLT => self.simple_instruction("LessThan", offset),
            Opcode::CmpLTEq => self.simple_instruction("LessThanEquals", offset),
            Opcode::CmpGT => self.simple_instruction("GreaterThan", offset),
            Opcode::CmpGTEq => self.simple_instruction("GreaterThanEquals", offset),
            Opcode::CmpEq => self.simple_instruction("EqualsTo", offset),
            Opcode::CmpNotEq => self.simple_instruction("NotEqual", offset),

            Opcode::JumpIfTrue => self.jump_instruction("JumpIfTrue", 1, offset),
            Opcode::JumpIfFalse => self.jump_instruction("JumpIfFalse", 1, offset),
            Opcode::Jump => self.jump_instruction("Jump", 1, offset),
            Opcode::Loop => self.jump_instruction("Loop", -1, offset),

            Opcode::Call => {
                self.write_instruction("Call", offset);

                let index = &self.function.chunk.code[offset + 1];
                let i_padding = " ".repeat(
                    self.function.chunk.constants.len().to_string().len() - index.to_string().len(),
                );
                print!("{}{}", index, i_padding);

                println!(" (arg_count: {})", index);

                offset + 2
            }

            Opcode::BuildArray => self.long_const_instruction("BuildArray", offset, false),
            Opcode::Closure => self.closure(offset),
            Opcode::BuildClass => self.simple_instruction("Class", offset),
            Opcode::BuildCon => self.simple_instruction("BuildCon", offset),
            Opcode::Print => self.simple_instruction("Print", offset),
            Opcode::Return => self.simple_instruction("Return", offset),
            Opcode::Import => self.simple_instruction("Import", offset),
        }
    }

    fn simple_instruction(&self, name: &str, offset: usize) -> usize {
        self.write_instruction(name, offset);
        println!();
        offset + 1
    }

    fn byte_instruction(&self, name: &str, offset: usize) -> usize {
        self.write_instruction(name, offset);
        let index = &self.function.chunk.code[offset + 1];

        let i_padding = " ".repeat(
            self.function.chunk.constants.len().to_string().len() - index.to_string().len(),
            //.checked_sub(index.to_string().len())
            //.unwrap_or(0),
        );
        print!("{}{}", index, i_padding);

        self.write_value(*index as usize);

        offset + 2
    }

    fn long_const_instruction(&self, name: &str, offset: usize, has_value: bool) -> usize {
        use std::convert::TryInto;
        self.write_instruction(name, offset);

        let bytes = self.function.chunk.code[offset + 1..offset + 5]
            .try_into()
            .unwrap_or_else(|_| panic!("Expected a slice of length {}.", 4));
        let index = u32::from_le_bytes(bytes);
        let i_padding = " ".repeat(
            self.function.chunk.constants.len().to_string().len() - index.to_string().len(),
        );
        print!("{}{}", index, i_padding);

        if has_value {
            self.write_value(index as usize);
        } else {
            println!();
        }

        offset + 5
    }

    fn upvalue_instruction(&self, name: &str, offset: usize) -> usize {
        use std::convert::TryInto;
        self.write_instruction(name, offset);

        let bytes: [u8; 4] = self.function.chunk.code[offset + 1..offset + 5]
            .try_into()
            .unwrap_or_else(|_| panic!("Expected a slice of length {}.", 4));

        let index = u32::from_le_bytes(bytes);

        print!("{}\n", index);

        offset + 5
    }

    fn jump_instruction(&self, name: &str, sign: i8, offset: usize) -> usize {
        self.write_instruction(name, offset);

        let byte1 = self.function.chunk.code[offset + 1];
        let byte2 = self.function.chunk.code[offset + 2];
        let jump = u16::from_le_bytes([byte1, byte2]);
        let i_padding =
            " ".repeat(self.function.chunk.code.len().to_string().len() - jump.to_string().len());

        println!(
            "{}{} -> {}",
            i_padding,
            offset,
            offset as i32 + 3_i32 + sign as i32 * jump as i32
        );

        offset + 3
    }

    fn write_instruction(&self, name: &str, offset: usize) {
        let padding =
            " ".repeat(self.function.chunk.code.len().to_string().len() - offset.to_string().len());
        print!("{}{}: {:<20}", padding, offset, name);
    }

    fn write_value(&self, index: usize) {
        println!(" ({})", self.function.chunk.constants[index]);
    }

    fn write_global(&self, name: &str, offset: usize) -> usize {
        use std::convert::TryInto;
        self.write_instruction(name, offset);

        let bytes = self.function.chunk.code[offset + 1..offset + 5]
            .try_into()
            .unwrap_or_else(|_| panic!("Expected a slice of length {}.", 4));
        let index = u32::from_le_bytes(bytes);
        let i_padding = " ".repeat(
            self.function.chunk.constants.len().to_string().len() - index.to_string().len(),
        );
        print!("{}{}", index, i_padding);

        let a = self.function.module.upgrade();
        println!(
            " ({})",
            a.unwrap().borrow().get_value_at_index(index as usize)
        );

        offset + 5
    }

    fn closure(&self, mut offset: usize) -> usize {
        self.write_instruction("Closure", offset);
        offset += 1;
        println!();
        let num_upvals = self.function.chunk.code[offset];// + 1];

        offset += 1;

        for _ in 0..num_upvals {
            //println!("{}; {}", self.function.chunk.code[offset], self.function.chunk.code[offset + 1]);

            let is_local = self.function.chunk.code[offset];

            self.write_instruction(&self.function.chunk.code[offset + 1].to_string(), offset + 1);

            if is_local == 0 {
                println!("  local")
            } else if is_local == 1 {
                println!("  upvalue");
            } else {
                panic!("something broke");
            }

            offset += 2;
        }

        offset
    }
}
