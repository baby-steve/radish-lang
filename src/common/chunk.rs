use std::fmt;

use crate::common::value::Value;

#[derive(Debug, PartialEq)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn add_constant(&mut self, value: Value) -> usize {
        let index = self.constants.len();
        self.constants.push(value);
        index
    }
}

impl fmt::Display for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Bytecode:\n    ")?;
        for op in &self.code {
            write!(f, "{}, ", op)?;
        }

        write!(f, "\nConstants:\n    ")?;
        for con in &self.constants {
            write!(f, "[ {} ]", con)?;
        }

        Ok(())
    }
}