use std::{convert::TryInto, rc::Rc};

use crate::{
    common::{Disassembler, Opcode},
    vm::trace::Trace,
    vm::value::{Class, Closure, Value},
};

use crate::vm::{CallFrame, VM};

impl VM {
    /// Create a new [`Trace`] with the given message, adding context to it.
    fn error(&mut self, message: impl ToString) -> Trace {
        let mut trace = Trace::new(message);

        while let Some(frame) = self.frames.pop() {
            trace.add_context(frame.closure.function.name.to_string());
        }

        trace
    }

    /// Return a reference to the top most frame on the call stack.
    #[inline]
    fn current_frame(&self) -> &CallFrame {
        &self.frames[self.frame_count - 1]
    }

    /// Return a mutatable reference to the top most frame on the call stack.
    #[inline]
    fn current_frame_mut(&mut self) -> &mut CallFrame {
        &mut self.frames[self.frame_count - 1]
    }

    /// Get the current callframe's instruction pointer.
    #[inline]
    fn ip(&self) -> usize {
        self.current_frame().ip
    }

    /// Create an [`Opcode`] from the next byte.
    #[inline]
    fn decode_opcode(&mut self) -> Opcode {
        let chunk = &self.current_frame().closure.function.chunk;

        let op = Opcode::from(chunk.code[self.ip()]);

        self.current_frame_mut().ip += 1;

        op
    }

    /// Return the next `u8` sized byte from the bytecode stream.
    #[inline]
    fn read_byte(&mut self) -> u8 {
        self.current_frame_mut().ip += 1;
        self.current_frame().closure.function.chunk.code[self.ip() - 1]
    }

    /// Build a `u16` number from the bytecode stream.
    #[inline]
    fn read_short(&mut self) -> u16 {
        let frame = &mut self.frames[self.frame_count - 1];
        frame.ip += 2;
        let byte1 = frame.closure.function.chunk.code[frame.ip - 2];
        let byte2 = frame.closure.function.chunk.code[frame.ip - 1];
        u16::from_le_bytes([byte1, byte2])
    }

    /// Build a `u32` number from the bytecode stream.
    #[inline]
    fn read_long(&mut self) -> u32 {
        let frame = &mut self.frames[self.frame_count - 1];

        frame.ip += 4;

        let bytes = frame.closure.function.chunk.code[frame.ip - 4..frame.ip]
            .try_into()
            .unwrap();

        u32::from_le_bytes(bytes)
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

    /// Check if the top [`Value`] on the stack is falsey.
    #[inline]
    fn is_falsey(&mut self) -> bool {
        matches!(
            self.stack.peek(),
            Some(Value::Nil) | Some(Value::Boolean(false))
        )
    }

    #[inline]
    fn call_value(&mut self, callee: Value, arg_count: usize) -> Result<(), Trace> {
        match callee {
            Value::Closure(fun) => self.call_function(fun, arg_count),
            _ => {
                let message = format!("'{}' is not callable", callee);
                let trace = self.error(message);
                Err(trace)
            }
        }
    }

    #[inline]
    pub(crate) fn call_function(&mut self, closure: Closure, arg_count: usize) -> Result<(), Trace> {
        let offset = self.stack.stack.len() - arg_count;
        let frame = CallFrame {
            closure: Closure::from(Rc::clone(&closure.function)),
            ip: 0,
            offset,
        };

        self.frames.push(frame);
        self.frame_count += 1;

        Ok(())
    }

    /// Create a closure from the value on top of the stack.
    #[inline]
    fn make_closure(&mut self) -> Result<(), Trace> {
        let function = self.stack.pop().into_function().unwrap();
        let closure = Closure { function };

        self.stack.push(Value::Closure(closure));

        Ok(())
    }

    /// Create a class from the value on top of the stack.
    #[inline]
    fn make_class(&mut self) -> Result<(), Trace> {
        let name = self.stack.pop().into_string().unwrap();

        let class = Class::new(&name);

        self.stack.push(Value::Class(Rc::new(class)));

        Ok(())
    }

    /// Create a constructor from the value on top of the stack.
    #[inline]
    fn make_constructor(&mut self) -> Result<(), Trace> {
        let constructor = self.stack.pop();

        let name = match &constructor {
            Value::Closure(closure) => closure.function.name.to_string(),
            _ => unreachable!("can only create a constructor from a closure"),
        };

        let class = self.stack.peek().unwrap().into_class().unwrap();

        class.constructors.borrow_mut().insert(name, constructor);

        Ok(())
    }

    #[inline]
    fn print(&mut self) -> Result<(), Trace> {
        let _msg = self.stack.pop();
        
        //self.config.stdout.write(&format!("{}", msg));
        //Ok(())

        todo!();
    }

    #[inline]
    fn load_local(&mut self) -> Result<(), Trace> {
        // This is the locals position on the stack relative to the
        // function being called. So for example when the stack looks like:
        // ```
        // [fun script][fun call]["hello"]["world"]
        // ^ script     ^ function being called  ^ local being loaded
        // ```
        // the local being loaded will have a relative index of (1).
        let relative_index = self.read_long() as usize;

        //dbg!(&relative_index);

        // The stack position of the current function being called.
        let offset = self.current_frame().offset;

        //dbg!(&offset);

        // To get the locals position on the stack, we then have to add the locals
        // relative position to the function's position.
        let slot_index = relative_index + offset - 1;

        //dbg!(&slot_index);

        self.stack
            .push(self.stack.stack[slot_index as usize].clone());

        Ok(())
    }

    #[inline]
    fn save_local(&mut self) -> Result<(), Trace> {
        let slot_index = self.read_long() as usize + self.current_frame().offset - 1;

        self.stack.stack[slot_index as usize] = self.stack.peek().unwrap();

        Ok(())
    }

    #[inline]
    fn load_global(&mut self) -> Result<(), Trace> {
        let index = self.read_long() as usize;
        //let value = self.last_module.borrow().get_var(index).clone();

        let value = self
            .current_frame()
            .closure
            .function
            .module
            .upgrade()
            .expect("something broke")
            .borrow()
            .get_var(index)
            .clone();

        self.stack.push(value);

        Ok(())
    }

    #[inline]
    fn save_global(&mut self) -> Result<(), Trace> {
        let index = self.read_long() as usize;

        //self.last_module
        //    .borrow_mut()
        //    .set_var(index, self.stack.peek().unwrap());

        self.current_frame()
            .closure
            .function
            .module
            .upgrade()
            .expect("something broke")
            .borrow_mut()
            .set_var(index, self.stack.peek().unwrap());

        Ok(())
    }

    #[inline]
    fn load_field(&mut self) -> Result<(), Trace> {
        let from = self.stack.pop();
        let field_name = self.stack.pop();

        match from {
            Value::Module(module) => {
                let index = module
                    .borrow()
                    .get_index(&field_name.into_string().unwrap().borrow())
                    .expect("no value at index");

                let val = module.borrow().get_var(index).clone();

                self.stack.push(val);
            }
            _ => unimplemented!("field access on {} is unsupported", from),
        }

        Ok(())
    }

    #[inline]
    fn save_field(&mut self) -> Result<(), Trace> {
        todo!()
    }

    #[inline]
    fn jump_if_true(&mut self) -> Result<(), Trace> {
        let offset = self.read_short();
        if !self.is_falsey() {
            self.current_frame_mut().ip += offset as usize;
        }

        Ok(())
    }

    #[inline]
    fn jump_if_false(&mut self) -> Result<(), Trace> {
        let offset = self.read_short();
        if self.is_falsey() {
            self.current_frame_mut().ip += offset as usize;
        }

        Ok(())
    }

    #[inline]
    fn jump(&mut self) -> Result<(), Trace> {
        self.current_frame_mut().ip += self.read_short() as usize;

        Ok(())
    }

    #[inline]
    fn loop_(&mut self) -> Result<(), Trace> {
        self.current_frame_mut().ip -= self.read_short() as usize;

        Ok(())
    }

    #[inline]
    fn import(&mut self) -> Result<(), Trace> {
        let path = self
            .stack
            .pop()
            .into_string()
            .expect("path is not a string");

        // TODO: find a better way to get this.
        let source_path = "";

        // TODO: figure out import semantics.
        // currently any import will only ever be runned once. While this seems
        // to be a common trend, I'm not sure if its the best choice.
        if self
            .resolver
            .is_cached(&*path.borrow(), Some(source_path))
        {
            return Ok(());
        };

        let module = match self.resolver.resolve(Some(source_path), &path.borrow(), &mut self.pipeline) {
            Ok(m) => m,
            Err(e) => {
                e.emit();
                let msg = "failed to load module";
                return Err(Trace::new(msg));
            }
        };

        // swap out the last module with the new one.
        let last_module = std::mem::replace(&mut self.last_module, module);

        // store the last module.
        self.modules.push(last_module);

        // load and run the module's top level code.
        let closure = Closure {
            function: self.last_module.borrow().entry(),
        };
        self.stack.push(Value::Closure(closure.clone()));
        self.call_function(closure, 0)?;

        Ok(())
    }

    pub(crate) fn run(&mut self) -> Result<Value, Trace> {
        macro_rules! binary_op {
            ($op:tt) => {{
                let b = self.stack.pop();
                let a = self.stack.pop();
                self.stack.push(Value::from(a $op b));
            }};
        }

        macro_rules! unary_op {
            ($op:tt) => {{
                let val = self.stack.pop();
                self.stack.push(Value::from($op val));
            }};
        }

        loop {
            if self.config.trace {
                let dis = Disassembler::new(
                    "script",
                    &self.frames[self.frame_count - 1].closure.function,
                );
                let offset = &self.frames[self.frame_count - 1].ip;
                dis.disassemble_instruction(*offset);
                print!("    ");
                for slot in &self.stack.stack {
                    print!("[ {} ]", &slot);
                }
                println!();
            }

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
                Opcode::Neg => unary_op!(-),
                Opcode::Not => unary_op!(!),
                Opcode::Add => binary_op!(+),
                Opcode::Sub => binary_op!(-),
                Opcode::Mul => binary_op!(*),
                Opcode::Div => binary_op!(/),
                Opcode::Rem => binary_op!(%),
                Opcode::CmpLT => binary_op!(<),
                Opcode::CmpGT => binary_op!(>),
                Opcode::CmpEq => binary_op!(==),
                Opcode::CmpLTEq => binary_op!(<=),
                Opcode::CmpGTEq => binary_op!(>=),
                Opcode::CmpNotEq => binary_op!(!=),
                Opcode::DefGlobal => {
                    let index = self.read_long() as usize;
                    self.last_module
                        .borrow_mut()
                        .set_var(index, self.stack.peek().unwrap());

                    self.stack.pop();
                }
                Opcode::LoadGlobal => self.load_global()?,
                Opcode::SaveGlobal => self.save_global()?,
                Opcode::LoadLocal => self.load_local()?,
                Opcode::SaveLocal => self.save_local()?,
                Opcode::GetCapture => todo!(),
                Opcode::SetCapture => todo!(),
                Opcode::LoadField => self.load_field()?,
                Opcode::SaveField => self.save_field()?,
                Opcode::JumpIfFalse => self.jump_if_false()?,
                Opcode::JumpIfTrue => self.jump_if_true()?,
                Opcode::Jump => self.jump()?,
                Opcode::Loop => self.loop_()?,
                Opcode::Closure => self.make_closure()?,
                Opcode::BuildClass => self.make_class()?,
                Opcode::BuildCon => self.make_constructor()?,
                Opcode::Print => self.print()?,
                Opcode::Import => self.import()?,
                Opcode::Call => {
                    let arg_count = self.read_byte() as usize;
                    let callee = self.stack.peek_n(arg_count + 1).unwrap();
                    self.call_value(callee, arg_count)?;
                }
                Opcode::Return => {
                    let result = self.stack.pop(); // pop return value

                    // if that was the last frame, exit the VM.
                    if self.frame_count - 1 == 0 {
                        //self.stack.pop();
                        return Ok(result);
                    }

                    self.frame_count -= 1;

                    while self.stack.stack.len() > self.frames[self.frame_count].offset {
                        self.stack.stack.pop();
                    }

                    // pop the function being called.
                    self.stack.pop();

                    // push the result back onto the stack.
                    self.stack.push(result);

                    self.frames.pop();

                    if !self.modules.is_empty() {
                        // The previous module
                        let last_module = self.modules.pop().unwrap();

                        let current_module = std::mem::replace(&mut self.last_module, last_module);

                        // FIXME: needlessly push and pop the `nil` value returned by the module.
                        self.stack.pop();

                        self.stack.push(Value::Module(current_module));
                    }
                }
            }

            if self.config.trace {
                let mut width = 0;

                print!("    ");
                for slot in &self.stack.stack {
                    width += 4 + &slot.to_string().len();

                    print!("[ {} ]", &slot);
                }

                let spacing = " ".repeat(100_usize.checked_sub(width).unwrap_or(10));

                let module_name = &self.last_module.borrow().name;

                print!("{}{}", spacing, module_name);

                println!();
            }
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
