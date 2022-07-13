use std::fmt;
use std::collections::HashMap;

use super::Value;

/// A chunk of bytecode and its associated data.
#[derive(Debug, PartialEq, Clone)]
pub struct Chunk {
    /// An array where each byte is either an Opcode or a number-stream.
    pub code: Vec<u8>,
    /// An array used to load constants.
    pub constants: Vec<Value>,
    /// Contains identifers mapped to thier location in the constants array.
    pub identifiers: HashMap<String, usize>,
}

impl Chunk {
    pub fn new(code: Vec<u8>, constants: Vec<Value>) -> Self {
        Chunk {
            code,
            constants,
            identifiers: HashMap::new(),
        }
    }

    /// Add a [`Value`] to this [`Chunk`]'s constants array, 
    /// returning its index.
    pub fn add_constant(&mut self, value: Value) -> usize {
        let index = self.constants.len();
        self.constants.push(value);
        index
    }

    /// Add an identifier to this [`Chunk`]'s constants array, 
    /// returning its index. If the identifier is already in the 
    /// array, then returns that index.
    pub fn add_identifier(&mut self, name: &str) -> usize {
        if let Some(val) = self.identifiers.get(name) {
            *val
        } else {
            let index = self.add_constant(Value::from(name));
            self.identifiers.insert(name.to_string(), index);
            index
        }
    }
}

impl Default for Chunk {
    fn default() -> Self {
        Chunk::new(vec![], vec![])
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
