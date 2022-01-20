use std::{
    collections::{
        HashMap,
        hash_map::Entry::{Vacant, Occupied},
    },
    convert::TryInto,
    rc::Rc,
};

use crate::{
    common::{
        chunk::Chunk,
        opcode::Opcode,
        value::Value,
    },
    vm::stack::Stack,
};

pub trait RadishFile {
    fn write(&self, msg: &str);
}

impl std::fmt::Debug for dyn RadishFile {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "<RadishFile>")
    }
}

#[derive(Debug)]
struct RadishIO;

impl RadishFile for RadishIO {
    fn write(&self, msg: &str) {
        print!("{}\n", msg);
    }
}

#[derive(Debug)]
pub struct RadishConfig {
    stdout: Rc<dyn RadishFile>,
}

impl RadishConfig {
    pub fn new() -> Rc<RadishConfig> {
        Rc::new(RadishConfig {
            stdout: Rc::new(RadishIO),
        })
    }

    pub fn with_stdout(stdout: Rc<dyn RadishFile>) -> Rc<RadishConfig> {
        Rc::new(RadishConfig { stdout })
    }
}

#[derive(Debug)]
pub struct VM {
    pub chunk: Chunk,
    pub stack: Stack,
    pub ip: usize,

    pub globals: HashMap<String, Value>,
    pub config: Rc<RadishConfig>
}

impl VM {
    pub fn new(config: &Rc<RadishConfig>) -> VM {
        VM {
            chunk: Chunk::default(),
            ip: 0,
            stack: Stack::new(),
            globals: HashMap::new(),
            config: Rc::clone(&config),
        }
    }

    pub fn interpret(&mut self, chunk: Chunk) {
        self.chunk = chunk;
        self.run();
    }

    fn run(&mut self) {
        loop {

            //for slot in &self.stack.stack {
            //    print!("[ {} ]", &slot);
            //}
            //print!("\n");

            match self.decode_opcode() {
                Opcode::LoadConst => {
                    let index = self.read_byte() as usize;
                    self.stack.push(self.chunk.constants[index].clone());
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
                }
                Opcode::GetLocal => {
                    let slot_index = self.read_long() as usize;
                    self.stack.push(self.stack.stack[slot_index].clone());
                }
                Opcode::SetLocal => {
                    let slot_index = self.read_long() as usize;
                    self.stack.stack[slot_index] = self.stack.peek().unwrap();
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
                    let msg = self.stack.pop().unwrap();
                    self.config.stdout.write(&format!("{}", msg));
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

    #[inline]
    fn read_byte(&mut self) -> u8 {
        self.ip += 1;
        self.chunk.code[self.ip - 1]
    }

    #[inline]
    fn read_long(&mut self) -> u32 {
        self.ip += 4;
        let bytes = self.chunk.code[self.ip - 4 as usize..self.ip as usize]
            .try_into()
            .expect(&format!("Expected a slice of length {}.", 4));
        
        u32::from_le_bytes(bytes)
    }

    #[inline]
    fn read_constant_long(&mut self) -> Value {
        let index = self.read_long() as usize;
        self.chunk.constants[index].clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_vm(chunk: Chunk) -> VM {
        let config = RadishConfig::new();
        let mut vm = VM::new(&config);
        vm.interpret(chunk);
        vm
    }

    #[test]
    fn test_halt_opcode() {
        let code = vec![Opcode::Halt as u8];
        let vm = test_vm(Chunk::new(code, vec![]));
        assert_eq!(vm.ip, 1);
    }

    #[test]
    fn test_constant_opcode() {
        let code = vec![Opcode::LoadConst as u8, 0, Opcode::Halt as u8];
        let constants = vec![Value::Number(123.0)];
        let mut vm = test_vm(Chunk::new(code, constants));
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
        let mut vm = test_vm(Chunk::new(code, constants));
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
        let mut vm = test_vm(Chunk::new(code, constants));
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
        let mut vm = test_vm(Chunk::new(code, constants));
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
        let mut vm = test_vm(Chunk::new(code, constants));
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
        let mut vm = test_vm(Chunk::new(code, constants));
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
        let mut vm = test_vm(Chunk::new(code, constants));
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
        let mut vm = test_vm(Chunk::new(code, constants));
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
        let mut vm = test_vm(Chunk::new(code, constants));
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
        let mut vm = test_vm(Chunk::new(code, constants));
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
        let mut vm = test_vm(Chunk::new(code, constants));
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
        let mut vm = test_vm(Chunk::new(code, constants));
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
        let mut vm = test_vm(Chunk::new(code, constants));
        assert_eq!(vm.stack.peek(), Some(Value::Number(-2.0)));
    }

    #[test]
    fn test_not_opcode() {
        let code = vec![
            Opcode::True as u8,
            Opcode::Not as u8,
            Opcode::Halt as u8
        ];
        let mut vm = test_vm(Chunk::new( code, vec![]));
        assert_eq!(vm.stack.peek(), Some(Value::from(false)));
    }

    #[test]
    fn test_true_opcode() {
        let code = vec!(Opcode::True as u8, Opcode::Halt as u8);
        let mut vm = test_vm(Chunk::new( code, vec![]));
        assert_eq!(vm.stack.peek(), Some(Value::Boolean(true)));
    }

    #[test]
    fn test_false_opcode() {
        let code = vec!(Opcode::False as u8, Opcode::Halt as u8);
        let mut vm = test_vm(Chunk::new( code, vec![]));
        assert_eq!(vm.stack.peek(), Some(Value::Boolean(false)));
    }

    #[test]
    fn test_nil_opcode() {
        let code = vec![Opcode::Nil as u8, Opcode::Halt as u8];
        let mut vm = test_vm(Chunk::new( code, vec![]));
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

        let vm = test_vm(Chunk::new(code, constants));

        println!("{:?}", vm.stack);
        println!("{:?}", vm.globals);
        assert_eq!(vm.globals.get("\"a\""), Some(&Value::from(23.0)));
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