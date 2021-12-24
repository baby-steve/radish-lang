use crate::opcode::Opcode;
use crate::value::Value;

pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
}

#[derive(Debug)]
pub struct Stack {
    stack: Vec<Value>,
}

impl Stack {
    fn new() -> Stack {
        Stack { stack: vec![] }
    }

    fn push(&mut self, val: Value) {
        self.stack.push(val);
    }

    fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }

    fn peek(&mut self) -> Option<Value> {
        if self.stack.len() <= 0 {
            None
        } else {
            Some(self.stack[self.stack.len() - 1].clone())
        }
    }
}

pub struct VM {
    pub chunk: Chunk,
    pub stack: Stack,
    pub ip: usize,
}

impl VM {
    pub fn new(chunk: Chunk) -> VM {
        VM {
            chunk,
            ip: 0,
            stack: Stack::new(),
        }
    }

    pub fn interpret(&mut self) {
        self.run();
    }

    fn run(&mut self) {
        loop {
            match self.decode_opcode() {
                Opcode::Constant => {
                    self.ip += 1;
                    self.stack
                        .push(self.chunk.constants[self.chunk.code[self.ip - 1] as usize].clone());
                }
                Opcode::Add => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(a + b);
                }
                Opcode::Subtract => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(a - b);
                },
                Opcode::Multiply => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(a * b);
                },
                Opcode::Divide => break,
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
        let code = vec![Opcode::Constant as u8, 0, Opcode::Halt as u8];
        let constants = vec![Value::Number(123.0)];
        let mut vm = VM::new(Chunk { code, constants });
        vm.run();
        assert_eq!(vm.stack.peek(), Some(Value::Number(123.0)));
    }

    #[test]
    fn test_add_opcode() {
        let code = vec![
            Opcode::Constant as u8, 0,
            Opcode::Constant as u8, 1,
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
            Opcode::Constant as u8, 0,
            Opcode::Constant as u8, 1,
            Opcode::Subtract as u8,
            Opcode::Halt as u8,
        ];
        let constants = vec![Value::Number(2.0), Value::Number(3.0)];
        let mut vm = VM::new(Chunk { code, constants });
        vm.run();
        assert_eq!(vm.stack.peek(), Some(Value::Number(1.0)));
    }

    #[test]
    fn test_multiply_opcode() {
        let code = vec![
            Opcode::Constant as u8, 0,
            Opcode::Constant as u8, 1,
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
            Opcode::Constant as u8, 0,
            Opcode::Constant as u8, 1,
            Opcode::Divide as u8,
            Opcode::Halt as u8,
        ];
        let constants = vec![Value::Number(4.0), Value::Number(2.0)];
        let mut vm = VM::new(Chunk { code, constants });
        vm.run();
        assert_eq!(vm.stack.peek(), Some(Value::Number(2.0)));
    }
}
