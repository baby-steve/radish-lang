use std::{
    cell::RefCell,
    collections::{
        hash_map::Entry::{Occupied, Vacant},
        HashMap,
    },
    convert::TryInto,
    rc::Rc,
};

use std::time::Instant;

use crate::{
    common::{
        chunk::Chunk, disassembler::Disassembler, opcode::Opcode, value::Function, value::Module,
        value::Value,
    },
    vm::stack::Stack,
};

pub trait RadishFile {
    fn write(&self, msg: &str);
}

impl std::fmt::Debug for dyn RadishFile {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "__RadishFile__")
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
pub struct CallFrame {
    /// Call frame's function.
    pub function: Rc<Function>,
    /// Track where we're at in the function's chunk.
    pub ip: usize,
    /// VM stack offset.
    pub offset: usize,
}

#[derive(Debug)]
pub struct VM {
    pub stack: Stack,
    pub frames: Vec<CallFrame>,
    pub frame_count: usize,

    pub config: Rc<RadishConfig>,

    pub last_module: Rc<RefCell<Module>>,
}

impl VM {
    pub fn new(config: &Rc<RadishConfig>, module: Rc<RefCell<Module>>) -> VM {
        VM {
            stack: Stack::new(),
            frames: Vec::new(),
            frame_count: 0,
            config: Rc::clone(&config),
            last_module: module,
        }
    }

    pub fn interpret(&mut self, script: Function) {
        let script = Rc::new(script);

        self.stack.push(Value::Function(Rc::clone(&script)));
        self.call_function(script, 0);

        let start = Instant::now();
        self.run();
        let duration = start.elapsed();

        println!("Time elapsed in run() is: {:?}", duration);
    }

    #[inline]
    fn decode_opcode(&mut self) -> Opcode {
        let chunk = &self.frames[self.frame_count - 1].function.chunk;
        let op = Opcode::from(chunk.code[self.frames[self.frame_count - 1].ip]);

        self.frames[self.frame_count - 1].ip += 1;
        return op;
    }

    #[inline]
    fn read_byte(&mut self) -> u8 {
        let frame = &mut self.frames[self.frame_count - 1];
        frame.ip += 1;
        frame.function.chunk.code[frame.ip - 1]
    }

    #[inline]
    fn read_short(&mut self) -> u16 {
        let frame = &mut self.frames[self.frame_count - 1];
        frame.ip += 2;
        let byte1 = frame.function.chunk.code[frame.ip - 2];
        let byte2 = frame.function.chunk.code[frame.ip - 1];
        u16::from_le_bytes([byte1, byte2])
    }

    #[inline]
    fn read_long(&mut self) -> u32 {
        let frame = &mut self.frames[self.frame_count - 1];
        frame.ip += 4;
        let bytes = frame.function.chunk.code[frame.ip - 4..frame.ip]
            .try_into()
            .expect(&format!("Expected a slice of length {}.", 4));
        u32::from_le_bytes(bytes)
    }

    #[inline]
    fn read_constant_long(&mut self) -> Value {
        let index = self.read_long() as usize;
        self.frames[self.frame_count - 1].function.chunk.constants[index].clone()
    }

    #[inline]
    fn is_falsey(&mut self) -> bool {
        match self.stack.peek() {
            Some(Value::Nil) | Some(Value::Boolean(false)) => true,
            _ => false,
        }
    }

    #[inline]
    fn call_value(&mut self, callee: Value, arg_count: usize) {
        match callee {
            Value::Function(fun) => self.call_function(fun, arg_count),
            _ => {
                println!("Value '{}' is not callable.", callee);
                panic!("Attempt to call an uncallable value.");
            }
        }
    }

    #[inline]
    fn call_function(&mut self, fun: Rc<Function>, arg_count: usize) {
        let offset = self.stack.stack.len() - arg_count;
        let frame = CallFrame {
            function: Rc::clone(&fun),
            ip: 0,
            offset,
        };

        self.frames.push(frame);
        self.frame_count += 1;
    }

    fn run(&mut self) {
        loop {
            // Todo: store current frame in local variable
            // let frame = &mut self.frames[self.frame_count]

            // let dis = Disassembler::new("script", &self.frames[self.frame_count - 1].function);
            // let offset = &self.frames[self.frame_count - 1].ip;
            //
            // dis.disassemble_instruction(*offset);
            // print!("    ");
            // for slot in &self.stack.stack {
            //     print!("[ {} ]", &slot);
            // }
            // print!("\n");

            match self.decode_opcode() {
                Opcode::LoadConst => {
                    let index = self.read_byte() as usize;
                    self.stack.push(
                        self.frames[self.frame_count - 1].function.chunk.constants[index].clone(),
                    );
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
                    let index = self.read_long() as usize;
                    self.last_module
                        .borrow_mut()
                        .set_var(index, self.stack.peek().unwrap());

                    self.stack.pop();
                }
                Opcode::GetGlobal => {
                    let index = self.read_long() as usize;
                    let value = self.last_module.borrow().get_var(index).clone();

                    self.stack.push(value);
                }
                Opcode::SetGlobal => {
                    let index = self.read_long() as usize;

                    self.last_module
                        .borrow_mut()
                        .set_var(index, self.stack.peek().unwrap());
                }
                Opcode::GetLocal => {
                    let slot_index =
                        self.read_long() as usize + self.frames[self.frame_count - 1].offset - 1;

                    self.stack
                        .push(self.stack.stack[slot_index as usize].clone());
                }
                Opcode::SetLocal => {
                    let slot_index =
                        self.read_long() as usize + self.frames[self.frame_count - 1].offset - 1;

                    self.stack.stack[slot_index as usize] = self.stack.peek().unwrap();
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
                }
                Opcode::Multiply => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a * b);
                }
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
                Opcode::JumpIfFalse => {
                    let offset = self.read_short();
                    if self.is_falsey() {
                        self.frames[self.frame_count - 1].ip += offset as usize;
                    }
                }
                Opcode::JumpIfTrue => {
                    let offset = self.read_short();
                    if !self.is_falsey() {
                        self.frames[self.frame_count - 1].ip += offset as usize;
                    }
                }
                Opcode::Jump => {
                    self.frames[self.frame_count - 1].ip += self.read_short() as usize;
                }
                Opcode::Loop => {
                    self.frames[self.frame_count - 1].ip -= self.read_short() as usize;
                }
                Opcode::Call => {
                    let arg_count = self.read_byte() as usize;
                    let callee = self.stack.peek_n(arg_count + 1).unwrap();
                    self.call_value(callee, arg_count);
                }
                Opcode::Print => {
                    let msg = self.stack.pop().unwrap();
                    self.config.stdout.write(&format!("{}", msg));
                }
                Opcode::Return => {
                    let result = self.stack.pop().unwrap();

                    if self.frame_count - 1 == 0 {
                        self.stack.pop();
                        return;
                    }

                    self.frame_count -= 1;

                    self.stack.pop();
                    self.stack.push(result);

                    self.frames.pop();
                }
            }

            // print!("    ");
            // for slot in &self.stack.stack {
            //     print!("[ {} ]", &slot);
            // }
            // print!("\n");
        }
    }
}
/*
#[cfg(test)]
mod tests {
    use super::*;

    fn test_vm(chunk: Chunk) -> VM<'static> {
        let script = Function {
            arity: 0,
            name: String::from("test").into_boxed_str(),
            chunk,
        };

        let config = RadishConfig::new();
        let mut vm = VM::new(&config);
        vm.interpret(&script);
        vm
    }

    /*#[test]
    fn test_halt_opcode() {
        let code = vec![Opcode::Return as u8];
        let vm = test_vm(Chunk::new(code, vec![]));
        assert_eq!(vm.ip, 1);
    }*/

    #[test]
    fn test_constant_opcode() {
        let code = vec![Opcode::LoadConst as u8, 0, Opcode::Return as u8];
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
            Opcode::Return as u8,
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
            Opcode::Return as u8,
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
            Opcode::Return as u8,
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
            Opcode::Return as u8,
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
            Opcode::Return as u8,
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
            Opcode::Return as u8,
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
            Opcode::Return as u8,
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
            Opcode::Return as u8,
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
            Opcode::Return as u8,
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
            Opcode::Return as u8,
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
            Opcode::Return as u8,
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
            Opcode::Return as u8
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
            Opcode::Return as u8
        ];
        let mut vm = test_vm(Chunk::new( code, vec![]));
        assert_eq!(vm.stack.peek(), Some(Value::from(false)));
    }

    #[test]
    fn test_true_opcode() {
        let code = vec!(Opcode::True as u8, Opcode::Return as u8);
        let mut vm = test_vm(Chunk::new( code, vec![]));
        assert_eq!(vm.stack.peek(), Some(Value::Boolean(true)));
    }

    #[test]
    fn test_false_opcode() {
        let code = vec!(Opcode::False as u8, Opcode::Return as u8);
        let mut vm = test_vm(Chunk::new( code, vec![]));
        assert_eq!(vm.stack.peek(), Some(Value::Boolean(false)));
    }

    #[test]
    fn test_nil_opcode() {
        let code = vec![Opcode::Nil as u8, Opcode::Return as u8];
        let mut vm = test_vm(Chunk::new( code, vec![]));
        assert_eq!(vm.stack.peek(), Some(Value::Nil));
    }

    #[test]
    fn test_define_global_opcode() {
        let code = vec![
            Opcode::LoadConst as u8, 1, // 23
            Opcode::DefGlobal as u8, 0, 0, 0, 0, // "a"
            Opcode::Return as u8,
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
            Opcode::Return as u8,
        ];

        let constants = vec![Value::from(23.0)];
    }*/
}*/
