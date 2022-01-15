use std::{
    collections::{
        HashMap,
        hash_map::Entry::{Vacant, Occupied},
    },
    convert::TryInto,
};

use crate::{
    common::{
        chunk::Chunk,
        opcode::Opcode,
        value::Value,
    },
};

#[derive(Debug, PartialEq)]
pub struct Stack {
    stack: Vec<Value>,
}

impl Stack {
    pub fn new() -> Stack {
        Stack { stack: vec![] }
    }

    pub fn push(&mut self, val: Value) {
        self.stack.push(val);
    }

    pub fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }

    pub fn peek(&mut self) -> Option<Value> {
        if self.stack.len() <= 0 {
            None
        } else {
            Some(self.stack[self.stack.len() - 1].clone())
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct VM {
    pub chunk: Chunk,
    pub stack: Stack,
    pub ip: usize,

    pub globals: HashMap<String, Value>,
}

impl VM {
    pub fn new(chunk: Chunk) -> VM {
        VM {
            chunk,
            ip: 0,
            stack: Stack::new(),
            globals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self) {
        self.run();
    }

    fn run(&mut self) {
        loop {

            for slot in &self.stack.stack {
                print!("[ {} ]", &slot);
            }
            print!("\n");

            match self.decode_opcode() {
                Opcode::LoadConst => {
                    self.ip += 1;
                    self.stack
                        .push(self.chunk.constants[self.chunk.code[self.ip - 1] as usize].clone());
                }
                Opcode::LoadConstLong => {
                    let constant = self.read_constant_long();
                    self.stack.push(constant);
                }
                Opcode::True => {
                    self.stack.push(Value::Boolean(true));
                }
                Opcode::False => {
                    self.stack.push(Value::Boolean(false));
                }
                Opcode::Nil => {
                    self.stack.push(Value::Nil);
                }
                Opcode::Pop => {
                    self.stack.pop();
                }
                Opcode::DefGlobal => {
                    let name = self.read_constant_long();
                    self.globals.insert(name.to_string(), self.stack.peek().unwrap());
                    self.stack.pop();
                }
                Opcode::GetGlobal => {
                    let name = self.read_constant_long();
                    match self.globals.get(&name.to_string()) {
                        Some(value) => self.stack.push(value.clone()),
                        None => panic!("Found an undefined global."),
                    }
                }
                Opcode::SetGlobal => {
                    let name = self.read_constant_long();
                    let entry = self.globals.entry(name.to_string());
                    match entry {
                        Occupied(mut val) => val.insert(self.stack.pop().unwrap()),
                        Vacant(_) => panic!("Cannot assign to a undefined global variable"),
                    };
                    println!("{:?}", self.globals);
                }
                Opcode::Negate => {
                    let value = self.stack.pop().unwrap();
                    self.stack.push(-value);
                }
                Opcode::Not => {
                    let value = self.stack.pop().unwrap();
                    self.stack.push(!value);
                }
                Opcode::Add => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a + b);
                }
                Opcode::Subtract => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a - b);
                },
                Opcode::Multiply => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a * b);
                },
                Opcode::Divide => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a / b);
                }
                Opcode::LessThan => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Boolean(a < b));
                }
                Opcode::LessThanEquals => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Boolean(a <= b));
                }
                Opcode::GreaterThan => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Boolean(a > b));
                }
                Opcode::GreaterThanEquals => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Boolean(a >= b));
                }
                Opcode::EqualsTo => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Boolean(a == b));
                }
                Opcode::NotEqual => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Boolean(a != b));
                }
                Opcode::Print => {
                    println!("{}", self.stack.pop().unwrap());
                }
                Opcode::Halt => {
                    break;
                }
            }
        }
    }

    fn decode_opcode(&mut self) -> Opcode {
        let op = Opcode::from(self.chunk.code[self.ip]);
        self.ip += 1;
        return op;
    }

    /// Reads a u32 index from [`code`] and returns the [`Value`] 
    /// at [`constants[index]`].
    fn read_constant_long(&mut self) -> Value {
        self.ip += 4;
        let bytes = self.chunk.code[self.ip - 4 as usize..self.ip as usize]
            .try_into()
            .expect(&format!("Expected a slice of length {}.", 4));
        
        self.chunk.constants[u32::from_le_bytes(bytes) as usize].clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_halt_opcode() {
        let code = vec![Opcode::Halt as u8];
        let mut vm = VM::new(Chunk {
            code,
            constants: vec![],
        });
        vm.run();
        assert_eq!(vm.ip, 1);
    }

    #[test]
    fn test_constant_opcode() {
        let code = vec![Opcode::LoadConst as u8, 0, Opcode::Halt as u8];
        let constants = vec![Value::Number(123.0)];
        let mut vm = VM::new(Chunk { code, constants });
        vm.run();
        assert_eq!(vm.stack.peek(), Some(Value::Number(123.0)));
    }

    #[test]
    fn test_add_opcode() {
        let code = vec![
            Opcode::LoadConst as u8, 0,
            Opcode::LoadConst as u8, 1,
            Opcode::Add as u8,
            Opcode::Halt as u8,
        ];
        let constants = vec![Value::Number(2.0), Value::Number(3.0)];
        let mut vm = VM::new(Chunk { code, constants });
        vm.run();
        assert_eq!(vm.stack.peek(), Some(Value::Number(5.0)));
    }

    #[test]
    fn test_subtract_opcode() {
        let code = vec![
            Opcode::LoadConst as u8, 0,
            Opcode::LoadConst as u8, 1,
            Opcode::Subtract as u8,
            Opcode::Halt as u8,
        ];
        let constants = vec![Value::Number(3.0), Value::Number(2.0)];
        let mut vm = VM::new(Chunk { code, constants });
        vm.run();
        assert_eq!(vm.stack.peek(), Some(Value::Number(1.0)));
    }

    #[test]
    fn test_multiply_opcode() {
        let code = vec![
            Opcode::LoadConst as u8, 0,
            Opcode::LoadConst as u8, 1,
            Opcode::Multiply as u8,
            Opcode::Halt as u8,
        ];
        let constants = vec![Value::Number(2.0), Value::Number(5.0)];
        let mut vm = VM::new(Chunk { code, constants });
        vm.run();
        assert_eq!(vm.stack.peek(), Some(Value::Number(10.0)));
    }

    #[test]
    fn test_divide_opcode() {
        let code = vec![
            Opcode::LoadConst as u8, 0,
            Opcode::LoadConst as u8, 1,
            Opcode::Divide as u8,
            Opcode::Halt as u8,
        ];
        let constants = vec![Value::Number(4.0), Value::Number(2.0)];
        let mut vm = VM::new(Chunk { code, constants });
        vm.run();
        assert_eq!(vm.stack.peek(), Some(Value::Number(2.0)));
    }
    
    #[test]
    fn test_less_than_opcode() {
        let code = vec![
            Opcode::LoadConst as u8, 0,
            Opcode::LoadConst as u8, 1,
            Opcode::LessThan as u8,
            Opcode::Halt as u8,            
        ];
        let constants = vec![Value::Number(4.0), Value::Number(5.0)];
        let mut vm = VM::new(Chunk { code, constants });
        vm.run();
        assert_eq!(vm.stack.peek(), Some(Value::Boolean(true)));
    }

    #[test]
    fn test_less_than_equals_opcode() {
        let code = vec![
            Opcode::LoadConst as u8, 0,
            Opcode::LoadConst as u8, 1,
            Opcode::LessThanEquals as u8,
            Opcode::Halt as u8,            
        ];
        let constants = vec![Value::Number(4.0), Value::Number(5.0)];
        let mut vm = VM::new(Chunk { code, constants });
        vm.run();
        assert_eq!(vm.stack.peek(), Some(Value::Boolean(true)));
    }

    #[test]
    fn test_greater_than_opcode() {
        let code = vec![
            Opcode::LoadConst as u8, 0,
            Opcode::LoadConst as u8, 1,
            Opcode::GreaterThan as u8,
            Opcode::Halt as u8,            
        ];
        let constants = vec![Value::Number(6.0), Value::Number(5.0)];
        let mut vm = VM::new(Chunk { code, constants });
        vm.run();
        assert_eq!(vm.stack.peek(), Some(Value::Boolean(true)));
    }

    #[test]
    fn test_greater_than_equal_opcode() {
        let code = vec![
            Opcode::LoadConst as u8, 0,
            Opcode::LoadConst as u8, 1,
            Opcode::GreaterThanEquals as u8,
            Opcode::Halt as u8,            
        ];
        let constants = vec![Value::Number(8.0), Value::Number(5.0)];
        let mut vm = VM::new(Chunk { code, constants });
        vm.run();
        assert_eq!(vm.stack.peek(), Some(Value::Boolean(true)));
    }

    #[test]
    fn test_equals_to_opcode() {
        let code = vec![
            Opcode::LoadConst as u8, 0,
            Opcode::LoadConst as u8, 1,
            Opcode::EqualsTo as u8,
            Opcode::Halt as u8,            
        ];
        let constants = vec![Value::Number(5.0), Value::Number(5.0)];
        let mut vm = VM::new(Chunk { code, constants });
        vm.run();
        assert_eq!(vm.stack.peek(), Some(Value::Boolean(true)));
    }

    #[test]
    fn test_not_equal_opcode() {
        let code = vec![
            Opcode::LoadConst as u8, 0,
            Opcode::LoadConst as u8, 1,
            Opcode::NotEqual as u8,
            Opcode::Halt as u8,            
        ];
        let constants = vec![Value::Number(9.0), Value::Number(5.0)];
        let mut vm = VM::new(Chunk { code, constants });
        vm.run();
        assert_eq!(vm.stack.peek(), Some(Value::Boolean(true)));
    }

    #[test]
    fn test_multiple_opcodes() {
        let code = vec![
            Opcode::LoadConst as u8, 0,
            Opcode::LoadConst as u8, 1,
            Opcode::Add as u8,
            Opcode::LoadConst as u8, 2,
            Opcode::LoadConst as u8, 3,
            Opcode::Divide as u8,
            Opcode::LoadConst as u8, 4,
            Opcode::Multiply as u8,
            Opcode::Subtract as u8,
            Opcode::Halt as u8,
        ];
        let constants = vec![
            Value::Number(12.0), 
            Value::Number(7.0),
            Value::Number(8.0),
            Value::Number(4.0),
            Value::Number(5.0),
        ];
        let mut vm = VM::new(Chunk { code, constants });
        vm.run();
        assert_eq!(vm.stack.peek(), Some(Value::Number(9.0)));
    }

    #[test]
    fn test_negate_opcode() {
        let code = vec![
            Opcode::LoadConst as u8, 0,
            Opcode::Negate as u8,
            Opcode::Halt as u8
        ];
        let constants = vec![Value::Number(2.0)];
        let mut vm = VM::new(Chunk { code, constants });
        vm.run();
        assert_eq!(vm.stack.peek(), Some(Value::Number(-2.0)));
    }

    #[test]
    fn test_not_opcode() {
        let code = vec![
            Opcode::True as u8,
            Opcode::Not as u8,
            Opcode::Halt as u8
        ];
        let mut vm = VM::new(Chunk { code, constants: vec![] });
        vm.run();
        assert_eq!(vm.stack.peek(), Some(Value::from(false)));
    }

    #[test]
    fn test_true_opcode() {
        let code = vec!(Opcode::True as u8, Opcode::Halt as u8);
        let mut vm = VM::new(Chunk { code, constants: vec!() });
        vm.run();
        assert_eq!(vm.stack.peek(), Some(Value::Boolean(true)));
    }

    #[test]
    fn test_false_opcode() {
        let code = vec!(Opcode::False as u8, Opcode::Halt as u8);
        let mut vm = VM::new(Chunk { code, constants: vec!() });
        vm.run();
        assert_eq!(vm.stack.peek(), Some(Value::Boolean(false)));
    }

    #[test]
    fn test_nil_opcode() {
        let code = vec![Opcode::Nil as u8, Opcode::Halt as u8];
        let mut vm = VM::new(Chunk { code, constants: vec![] });
        vm.run();
        assert_eq!(vm.stack.peek(), Some(Value::Nil));
    }

    #[test]
    fn test_define_global_opcode() {
        let code = vec![
            Opcode::LoadConst as u8, 1, // 23
            Opcode::DefGlobal as u8, 0, 0, 0, 0, // "a"
            Opcode::Halt as u8,
        ];

        let constants = vec![Value::from("a"), Value::from(23.0)];

        let mut vm = VM::new(Chunk {code, constants});
        vm.run();

        println!("{:?}", vm.stack);
        println!("{:?}", vm.globals);
        assert_eq!(vm.globals.get("a"), Some(&Value::from(23.0)));
    }
    /*
    #[test]
    fn test_print_opcode() {
        let code = vec![
            Opcode::LoadConst as u8, 0,
            Opcode::Print as u8,
            Opcode::Halt as u8,
        ];

        let constants = vec![Value::from(23.0)];
    }*/
}
