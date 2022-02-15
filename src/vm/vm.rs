use std::{cell::RefCell, convert::TryInto, rc::Rc};

use crate::{
    common::{
        disassembler::Disassembler, opcode::Opcode, value::Closure, value::Function,
        value::Module, value::Value,
    },
    vm::stack::Stack,
};

use crate::{RadishConfig};

#[derive(Debug)]
pub struct CallFrame {
    /// Call frame's function.
    //pub function: Rc<Function>,
    pub closure: Closure,
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
    pub fn new(config: &Rc<RadishConfig>) -> VM {
        VM {
            stack: Stack::new(),
            frames: Vec::new(),
            frame_count: 0,
            config: Rc::clone(&config),
            // HACK: this needs to be refactored.
            last_module: Module::new(""),
        }
    }

    pub fn interpret(&mut self, script: Function, module: Rc<RefCell<Module>>) {
        self.last_module = module;

        let closure = Closure {
            function: Rc::new(script),
        };
        self.stack.push(Value::Closure(closure.clone()));
        self.call_function(closure, 0);

        self.run();
    }

    #[inline]
    fn decode_opcode(&mut self) -> Opcode {
        let chunk = &self.frames[self.frame_count - 1].closure.function.chunk;
        let op = Opcode::from(chunk.code[self.frames[self.frame_count - 1].ip]);

        self.frames[self.frame_count - 1].ip += 1;
        return op;
    }

    #[inline]
    fn read_byte(&mut self) -> u8 {
        let frame = &mut self.frames[self.frame_count - 1];
        frame.ip += 1;
        frame.closure.function.chunk.code[frame.ip - 1]
    }

    #[inline]
    fn read_short(&mut self) -> u16 {
        let frame = &mut self.frames[self.frame_count - 1];
        frame.ip += 2;
        let byte1 = frame.closure.function.chunk.code[frame.ip - 2];
        let byte2 = frame.closure.function.chunk.code[frame.ip - 1];
        u16::from_le_bytes([byte1, byte2])
    }

    #[inline]
    fn read_long(&mut self) -> u32 {
        //dbg!("...");
        let frame = &mut self.frames[self.frame_count - 1];
        frame.ip += 4;
        let bytes = frame.closure.function.chunk.code[frame.ip - 4..frame.ip]
            .try_into()
            .expect(&format!("Expected a slice of length {}.", 4));
        //dbg!("why??");

        let bytes = u32::from_le_bytes(bytes);

        //dbg!("did it get here?");

        bytes
    }

    #[inline]
    fn read_constant_long(&mut self) -> Value {
        let index = self.read_long() as usize;
        self.frames[self.frame_count - 1]
            .closure
            .function
            .chunk
            .constants[index]
            .clone()
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
            Value::Closure(fun) => self.call_function(fun, arg_count),
            //Value::Function(fun) => self.call_function(fun, arg_count),
            //Value::Closure(closure) => self.call_closure(closure, arg_count),
            _ => {
                println!("Value '{}' is not callable.", callee);
                panic!("Attempt to call an uncallable value.");
            }
        }
    }

    #[inline]
    fn call_function(&mut self, closure: Closure, arg_count: usize) {
        let offset = self.stack.stack.len() - arg_count;
        let frame = CallFrame {
            closure: Closure::from(Rc::clone(&closure.function)),
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

            // let dis = Disassembler::new(
            //     "script",
            //     &self.frames[self.frame_count - 1].closure.function,
            // );
            // let offset = &self.frames[self.frame_count - 1].ip;
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
                        self.frames[self.frame_count - 1]
                            .closure
                            .function
                            .chunk
                            .constants[index]
                            .clone(),
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
                Opcode::Del => {
                    self.stack.pop();
                }
                Opcode::DefGlobal => {
                    let index = self.read_long() as usize;
                    self.last_module
                        .borrow_mut()
                        .set_var(index, self.stack.peek().unwrap());

                    self.stack.pop();
                }
                Opcode::LoadGlobal => {
                    let index = self.read_long() as usize;
                    let value = self.last_module.borrow().get_var(index).clone();

                    self.stack.push(value);
                }
                Opcode::SaveGlobal => {
                    let index = self.read_long() as usize;

                    self.last_module
                        .borrow_mut()
                        .set_var(index, self.stack.peek().unwrap());
                }
                Opcode::LoadLocal => {
                    let slot_index =
                        self.read_long() as usize + self.frames[self.frame_count - 1].offset - 1;

                    self.stack
                        .push(self.stack.stack[slot_index as usize].clone());
                }
                Opcode::SaveLocal => {
                    let slot_index =
                        self.read_long() as usize + self.frames[self.frame_count - 1].offset - 1;

                    self.stack.stack[slot_index as usize] = self.stack.peek().unwrap();
                }
                Opcode::GetCapture => todo!(),
                Opcode::SetCapture => todo!(),
                Opcode::Neg => {
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
                Opcode::Sub => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a - b);
                }
                Opcode::Mul => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a * b);
                }
                Opcode::Div => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a / b);
                }
                Opcode::Rem => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a % b);
                }
                Opcode::CmpLT => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Boolean(a < b));
                }
                Opcode::CmpLTEq => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Boolean(a <= b));
                }
                Opcode::CmpGT => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Boolean(a > b));
                }
                Opcode::CmpGTEq => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Boolean(a >= b));
                }
                Opcode::CmpEq => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Boolean(a == b));
                }
                Opcode::CmpNotEq => {
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
                Opcode::Closure => {
                    let function = self.stack.pop().unwrap();
                    let closure = Closure {
                        function: match function {
                            Value::Function(f) => f,
                            _ => {
                                println!("{}", function);
                                unreachable!("can only create a closure from a function");
                            }
                        },
                    };
                    self.stack.push(Value::Closure(closure));
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
