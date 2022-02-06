use crate::{
    common::{
        chunk::Chunk, disassembler::Disassembler, opcode::Opcode, span::Span,
        value::Function as FunctionValue, value::Module as ModuleValue, value::Value,
    },
    compiler::{ast::*, table::SymbolTable, visitor::Visitor},
};

use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub struct Local {
    name: String,
    depth: usize,
}

/// Track the state of a loop.
struct Loop {
    /// The start of the loop, used by `continue` statements.
    loop_start: usize,
    /// Placeholders emitted by `break` statements within the loop,
    /// back patched once the loop is exited.
    jump_placeholders: Vec<usize>,
}

impl Loop {
    pub fn new(loop_start: usize) -> Loop {
        Loop {
            loop_start,
            jump_placeholders: vec![],
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum FunctionType {
    Function,
    Script,
}

/// Track the state of the current `[Function]` being compiled.
#[derive(Debug)]
struct Frame {
    pub function: FunctionValue,
    pub function_type: FunctionType,
}

impl Frame {
    /// Helper method for creating a frame with a function type.
    pub fn function(fun: FunctionValue) -> Frame {
        Frame {
            function: fun,
            function_type: FunctionType::Function,
        }
    }

    /// Helper method for creating a frame with a script type.
    pub fn script(script: FunctionValue) -> Frame {
        Frame {
            function: script,
            function_type: FunctionType::Script,
        }
    }
}

pub struct Compiler<'a> {
    scope_depth: usize,
    locals: Vec<Local>,
    loops: Vec<Loop>,
    frame_count: usize,
    frame: Vec<Frame>,
    scope: &'a SymbolTable,

    module: Rc<RefCell<ModuleValue>>,
}

impl<'a> Compiler<'a> {
    pub fn new(scope: &'a SymbolTable) -> Compiler<'a> {
        Compiler {
            scope_depth: 0,
            locals: vec![],
            loops: vec![],
            frame_count: 0,
            frame: vec![],
            scope,
            module: ModuleValue::new("test"),
        }
    }

    pub fn compile(&mut self, ast: &AST, scope: &'a SymbolTable) -> Result<(Rc<RefCell<ModuleValue>>, FunctionValue), String> {
        self.scope = scope;

        let script = FunctionValue {
            arity: 0,
            chunk: Chunk::default(),
            name: String::from("").into_boxed_str(),
            module: Rc::downgrade(&self.module),
        };

        let frame = Frame::script(script);

        self.frame.push(frame);

        self.add_local("");

        self.declare_globals(&ast)?;

        for node in &ast.items {
            match self.statement(node) {
                Ok(_) => continue,
                Err(_) => continue,
            }
        }

        self.emit_return();

        Ok((self.module.clone(), self.frame.pop().unwrap().function))
    }

    /// Write a unsigned byte to the current `[Chunk]` being compiled.
    fn emit_byte(&mut self, byte: u8) {
        // should also emit the byte's location in source?
        let chunk = &mut self.frame[self.frame_count].function.chunk;
        chunk.code.push(byte);
    }

    /// Write two unsigned bytes to the current `[Chunk]` being compiled.
    fn emit_bytes(&mut self, byte_1: u8, byte_2: u8) {
        self.emit_byte(byte_1);
        self.emit_byte(byte_2);
    }

    /// Emit a return opcode with a `nil` return value.
    fn emit_return(&mut self) {
        self.emit_byte(Opcode::Nil as u8);
        self.emit_byte(Opcode::Return as u8);
    }

    /// emit the given bytecode instruction and write a placeholder
    /// for the jump offset.
    fn emit_jump(&mut self, instruction: Opcode) -> usize {
        self.emit_byte(instruction as u8);
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        self.last_byte() - 2
    }

    /// Replace a byte at a given offset with the supplied byte.
    fn replace_byte(&mut self, offset: usize, byte: u8) {
        let chunk = &mut self.frame[self.frame_count].function.chunk;
        chunk.code[offset] = byte;
    }

    /// Index of the last emitted byte.
    fn last_byte(&mut self) -> usize {
        self.frame[self.frame_count].function.chunk.code.len()
    }

    /// Go back into the bytecode stream and replace the operand
    /// at the given location with the calculated jump offset.
    fn patch_jump(&mut self, offset: usize) {
        let jump = self.last_byte() - offset - 2;

        if jump > u16::MAX.into() {
            // should probably test this and also make better error message.
            panic!("To much code to jump over.");
        }

        let bytes = (jump as u16).to_le_bytes();

        self.replace_byte(offset, bytes[0]);
        self.replace_byte(offset + 1, bytes[1]);
    }

    /// Emit a loop instruction, which jumps backwards by
    /// the given offset.
    fn emit_loop(&mut self, loop_start: usize) {
        self.emit_byte(Opcode::Loop as u8);

        let offset = self.last_byte() - loop_start + 2;
        if offset > u16::MAX.into() {
            // like jumps, I should probably test this and add better error message.
            panic!("To much code to jump over.");
        }

        let bytes = (offset as u16).to_le_bytes();

        self.emit_bytes(bytes[0], bytes[1]);
    }

    /// add a constant to the chunk's constant array. Returns the
    /// constant's index in the constant array as a u32.
    fn make_constant(&mut self, value: Value) -> u32 {
        let chunk = &mut self.frame[self.frame_count].function.chunk;
        chunk.add_constant(value) as u32
    }

    /// Write a `[Value]` to the current chunk begin compiled.
    fn emit_constant(&mut self, value: Value) {
        let index = self.make_constant(value);

        if index > 255 {
            self.emit_byte(Opcode::LoadConstLong as u8);

            let bytes = index.to_le_bytes();

            for byte in bytes {
                self.emit_byte(byte);
            }
        } else {
            self.emit_bytes(Opcode::LoadConst as u8, index as u8);
        }
    }

    /// Define a variable.
    fn define_variable(&mut self, name: &str) {
        if self.scope_depth > 0 {
            self.add_local(name);
        } else {
            let index = self.module.borrow_mut().get_index(name).unwrap();
            self.define_global(index as u32);
        }
    }

    /// Add a [`Local`] to the [`Compiler`]'s locals array.
    fn add_local(&mut self, name: &str) {
        let local = Local {
            name: name.to_string(),
            depth: self.scope_depth,
        };

        self.locals.push(local);
    }

    /// Define a global variable.
    fn define_global(&mut self, global: u32) {
        self.emit_byte(Opcode::DefGlobal as u8);

        for byte in global.to_le_bytes() {
            self.emit_byte(byte);
        }
    }

    /// Emit an `[Opcode]` to load the variable with the given name.
    fn load_variable(&mut self, name: &str) {
        let arg = match self.resolve_local(name) {
            Some(index) => {
                self.emit_byte(Opcode::GetLocal as u8);
                index as u32
            }
            None => {
                self.emit_byte(Opcode::GetGlobal as u8);
                let index = self.module.borrow().get_index(name).unwrap();
                index as u32
            }
        };

        for byte in arg.to_le_bytes() {
            self.emit_byte(byte);
        }
    }

    /// Emit an `[Opcode]` to save a value to the variable with the
    /// given name.
    fn save_variable(&mut self, name: &str) {
        let arg = match self.resolve_local(name) {
            Some(index) => {
                self.emit_byte(Opcode::SetLocal as u8);
                index as u32
            }
            None => {
                self.emit_byte(Opcode::SetGlobal as u8);
                //self.identifier_constant(name)

                let index = self.module.borrow().get_index(name).unwrap();
                index as u32
            }
        };

        for byte in arg.to_le_bytes() {
            self.emit_byte(byte);
        }
    }

    /// Resolve a local variable with the given name. Returns the variable's
    /// index in the `[Local]` array if it's locally defined or None if it's a global.
    fn resolve_local(&mut self, name: &str) -> Option<usize> {
        let mut index = self.locals.len();

        while index > 0 {
            let local = &self.locals[index - 1];

            if local.name == name {
                return Some(index - 1);
            }

            index -= 1;
        }

        None
    }

    /// Enter a new block level scope.
    fn enter_scope(&mut self) {
        self.scope_depth += 1
    }

    /// Leave the current scope, removing all locally declared variables.
    fn leave_scope(&mut self) {
        self.scope_depth -= 1;

        while self.locals.len() > 0 && self.locals[self.locals.len() - 1].depth > self.scope_depth {
            self.emit_byte(Opcode::Pop as u8);
            self.locals.pop();
        }
    }

    /// Enter a loop body.
    fn enter_loop(&mut self, index: usize) {
        self.loops.push(Loop::new(index));
    }

    /// Leave the current loop body, patching all jump offsets emitted by
    /// break statements in the loop body.
    fn leave_loop(&mut self) {
        let last_loop = self.loops.pop().unwrap();

        for jump_offset in last_loop.jump_placeholders {
            self.patch_jump(jump_offset);
        }
    }

    /// Enter a function body.
    fn enter_function(&mut self, frame: Frame) {
        self.frame.push(frame);
        self.frame_count += 1;
        self.enter_scope();
    }

    /// Leave the current function body.
    fn leave_function(&mut self) -> Frame {
        self.leave_scope();

        self.emit_return();
        self.frame_count -= 1;
        self.frame.pop().unwrap()
    }

    /// Sort of foward declare all globally scoped functions.
    fn declare_globals(&mut self, ast: &AST) -> Result<(), String> {
        // add all global declarations to the module with a value of nil.
        for (name, _) in self.scope.all().iter() {
            self.module.borrow_mut().add_var(name.clone());
        }

        for node in &ast.items {
            match node {
                Stmt::FunDeclaration(fun, _) => {
                    // get the function's location in the module's variables array.
                    let index = self.module.borrow_mut().get_index(&fun.id.name).unwrap();
                    // compile the functions body.
                    self.function(&fun)?;
                    // set the location in the module's variable array to the function's body.
                    self.define_global(index as u32);
                }
                _ => continue,
            }
        }

        Ok(())
    }

    /// Compile a function declaration,
    fn function(&mut self, fun: &Function) -> Result<(), String> {
        if fun.params.len() > u8::MAX.into() {
            panic!("Cannot have more than 255 parameters");
        }

        let frame = FunctionValue {
            arity: fun.params.len() as u8,
            name: fun.id.name.clone().into_boxed_str(),
            chunk: Chunk::default(),
            module: Rc::downgrade(&self.module),
        };

        self.enter_function(Frame::function(frame));
        {
            for param in &fun.params {
                self.define_variable(&param.name);
            }

            for stmt in fun.body.iter() {
                self.statement(stmt)?;
            }
        }

        let frame = self.leave_function();

        Disassembler::disassemble_chunk(&frame.function.name, &frame.function);
        self.emit_constant(Value::from(frame.function));

        Ok(())
    }
}

impl Visitor<(), String> for Compiler<'_> {
    fn function_declaration(&mut self, fun: &Function) -> Result<(), String> {
        // at this point all global functions have already been compiled,
        // so only locally scoped functions have to be handled.
        if self.scope_depth > 0 {
            self.function(fun)?;
            self.define_variable(&fun.id.name);
        }

        Ok(())
    }

    fn if_statement(
        &mut self,
        expr: &Expr,
        body: &Vec<Stmt>,
        else_branch: &Option<Box<Stmt>>,
    ) -> Result<(), String> {
        self.expression(&expr)?;
        let then_jump = self.emit_jump(Opcode::JumpIfFalse);
        self.emit_byte(Opcode::Pop as u8);
        self.block(&body)?;

        let else_jump = self.emit_jump(Opcode::Jump);

        self.patch_jump(then_jump);
        self.emit_byte(Opcode::Pop as u8);

        if let Some(else_branch) = &else_branch {
            self.statement(&else_branch)?;
        }

        self.patch_jump(else_jump);

        Ok(())
    }

    fn loop_statement(&mut self, body: &Vec<Stmt>) -> Result<(), String> {
        let loop_start = self.last_byte();
        self.enter_loop(loop_start);

        self.block(&body)?;
        self.emit_loop(loop_start);

        self.leave_loop();

        Ok(())
    }

    fn while_statement(&mut self, expr: &Expr, body: &Vec<Stmt>) -> Result<(), String> {
        let loop_start = self.last_byte();
        self.enter_loop(loop_start);

        self.expression(&expr)?;
        let exit_jump = self.emit_jump(Opcode::JumpIfFalse);
        self.emit_byte(Opcode::Pop as u8);

        self.block(&body)?;

        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.leave_loop();
        self.emit_byte(Opcode::Pop as u8);

        Ok(())
    }

    fn block(&mut self, body: &Vec<Stmt>) -> Result<(), String> {
        self.enter_scope();

        for node in body {
            self.statement(&node)?;
        }

        self.leave_scope();

        Ok(())
    }

    fn return_statement(&mut self, return_val: &Option<Expr>) -> Result<(), String> {
        if self.scope_depth == 0 {
            panic!("Return statement outside of a function");
        }

        if let Some(expr) = return_val {
            self.expression(expr)?;
        } else {
            self.emit_byte(Opcode::Nil as u8);
        }

        self.emit_byte(Opcode::Return as u8);

        Ok(())
    }

    fn break_statement(&mut self, _: &Span) -> Result<(), String> {
        let exit_jump = self.emit_jump(Opcode::Jump);
        self.emit_byte(Opcode::Pop as u8);

        let index = &self.loops.len() - 1;
        self.loops[index].jump_placeholders.push(exit_jump);

        Ok(())
    }

    fn continue_statement(&mut self, _: &Span) -> Result<(), String> {
        let loop_start = self.loops.last().unwrap().loop_start;

        self.emit_loop(loop_start);

        Ok(())
    }

    fn print(&mut self, expr: &Expr) -> Result<(), String> {
        self.expression(&expr)?;

        self.emit_byte(Opcode::Print as u8);

        Ok(())
    }

    fn var_declaration(&mut self, id: &Ident, init: &Option<Expr>) -> Result<(), String> {
        if self.scope_depth > 0 {
            if let Some(expr) = &init {
                self.expression(&expr)?;
            } else {
                self.emit_byte(Opcode::Nil as u8);
            }

            self.add_local(&id.name);
        } else {
            let index = self.module.borrow_mut().get_index(&id.name).unwrap();

            if let Some(expr) = &init {
                self.expression(&expr)?;
            } else {
                self.emit_byte(Opcode::Nil as u8);
            }

            self.define_global(index as u32);
        }

        Ok(())
    }

    fn assignment(&mut self, id: &Ident, op: &OpAssignment, expr: &Expr) -> Result<(), String> {
        match op {
            OpAssignment::PlusEquals => {
                self.load_variable(&id.name);
                self.expression(&expr)?;
                self.emit_byte(Opcode::Add as u8);
            }
            OpAssignment::MinusEquals => {
                self.load_variable(&id.name);
                self.expression(&expr)?;
                self.emit_byte(Opcode::Subtract as u8);
            }
            OpAssignment::MultiplyEquals => {
                self.load_variable(&id.name);
                self.expression(&expr)?;
                self.emit_byte(Opcode::Multiply as u8);
            }
            OpAssignment::DivideEquals => {
                self.load_variable(&id.name);
                self.expression(&expr)?;
                self.emit_byte(Opcode::Divide as u8);
            }
            OpAssignment::Equals => {
                self.expression(&expr)?;
            }
        };

        self.save_variable(&id.name);

        Ok(())
    }

    fn expression_stmt(&mut self, expr: &Expr) -> Result<(), String> {
        self.expression(&expr)?;

        self.emit_byte(Opcode::Pop as u8);

        Ok(())
    }

    fn binary_expression(&mut self, expr: &BinaryExpr) -> Result<(), String> {
        self.expression(&expr.left)?;
        self.expression(&expr.right)?;

        match &expr.op {
            Op::Add => self.emit_byte(Opcode::Add as u8),
            Op::Subtract => self.emit_byte(Opcode::Subtract as u8),
            Op::Multiply => self.emit_byte(Opcode::Multiply as u8),
            Op::Divide => self.emit_byte(Opcode::Divide as u8),
            Op::LessThan => self.emit_byte(Opcode::LessThan as u8),
            Op::LessThanEquals => self.emit_byte(Opcode::LessThanEquals as u8),
            Op::GreaterThan => self.emit_byte(Opcode::GreaterThan as u8),
            Op::GreaterThanEquals => self.emit_byte(Opcode::GreaterThanEquals as u8),
            Op::EqualsTo => self.emit_byte(Opcode::EqualsTo as u8),
            Op::NotEqual => self.emit_byte(Opcode::NotEqual as u8),
            _ => unreachable!("{:?} is not a binary operator.", &expr.op),
        }

        Ok(())
    }

    fn logical_expr(&mut self, expr: &BinaryExpr) -> Result<(), String> {
        self.expression(&expr.left)?;

        let op = match &expr.op {
            Op::And => Opcode::JumpIfFalse,
            Op::Or => Opcode::JumpIfTrue,
            _ => unreachable!("Invalid logical operator."),
        };

        let end_jump = self.emit_jump(op);
        self.emit_byte(Opcode::Pop as u8);

        self.expression(&expr.right)?;

        self.patch_jump(end_jump);

        Ok(())
    }

    fn unary(&mut self, arg: &Expr, op: &Op) -> Result<(), String> {
        self.expression(&arg)?;

        match op {
            Op::Subtract => self.emit_byte(Opcode::Negate as u8),
            Op::Bang => self.emit_byte(Opcode::Not as u8),
            _ => unreachable!("{:?} is not an unary operator.", &op),
        }

        Ok(())
    }

    fn call_expr(&mut self, callee: &Expr, args: &Vec<Box<Expr>>) -> Result<(), String> {
        if args.len() > u8::MAX.into() {
            panic!("Cannot have more than 255 arguments.");
        }

        self.expression(&callee)?;

        for arg in args.iter() {
            self.expression(arg)?;
        }

        self.emit_bytes(Opcode::Call as u8, args.len() as u8);

        Ok(())
    }

    fn identifier(&mut self, id: &Ident) -> Result<(), String> {
        self.load_variable(&id.name);
        Ok(())
    }

    fn number(&mut self, val: &f64) -> Result<(), String> {
        self.emit_constant(Value::Number(*val));
        Ok(())
    }

    fn string(&mut self, val: &str) -> Result<(), String> {
        self.emit_constant(Value::from(val));
        Ok(())
    }

    fn boolean(&mut self, val: &bool) -> Result<(), String> {
        match val {
            true => self.emit_byte(Opcode::True as u8),
            false => self.emit_byte(Opcode::False as u8),
        }
        Ok(())
    }

    fn nil(&mut self) -> Result<(), String> {
        self.emit_byte(Opcode::Nil as u8);
        Ok(())
    }
}
/*
#[cfg(test)]
mod tests {
    use super::*;

    use std::rc::Rc;

    use crate::common::source::Source;
    use crate::compiler::parser::Parser;

    fn run_test_compiler(test_string: &str) -> Compiler {
        let source = Source::source(test_string);
        let result = Parser::new(Rc::clone(&source)).parse().unwrap();
        let mut compiler = Compiler::new();
        compiler.run(&result);
        compiler
    }

    #[test]
    fn compile_binary_add_expr() {
        let result = run_test_compiler("1 + 23");
        assert_eq!(
            result.chunk.code,
            vec!(
                Opcode::LoadConst as u8,
                0,
                Opcode::LoadConst as u8,
                1,
                Opcode::Add as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
            result.chunk.constants,
            vec!(Value::Number(1.0), Value::Number(23.0),)
        );
    }

    #[test]
    fn compile_binary_sub_expr() {
        let result = run_test_compiler("12 - 6");
        assert_eq!(
            result.chunk.code,
            vec!(
                Opcode::LoadConst as u8,
                0,
                Opcode::LoadConst as u8,
                1,
                Opcode::Subtract as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
            result.chunk.constants,
            vec!(Value::Number(12.0), Value::Number(6.0),)
        );
    }

    #[test]
    fn compile_binary_mul_expr() {
        let result = run_test_compiler("5 * 2");
        assert_eq!(
            result.chunk.code,
            vec!(
                Opcode::LoadConst as u8,
                0,
                Opcode::LoadConst as u8,
                1,
                Opcode::Multiply as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
            result.chunk.constants,
            vec!(Value::Number(5.0), Value::Number(2.0),)
        );
    }

    #[test]
    fn compile_binary_div_expr() {
        let result = run_test_compiler("12 / 4");
        assert_eq!(
            result.chunk.code,
            vec!(
                Opcode::LoadConst as u8,
                0,
                Opcode::LoadConst as u8,
                1,
                Opcode::Divide as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
            result.chunk.constants,
            vec!(Value::Number(12.0), Value::Number(4.0),)
        );
    }

    #[test]
    fn compile_less_than_expr() {
        let result = run_test_compiler("5 < 2");
        assert_eq!(
            result.chunk.code,
            vec!(
                Opcode::LoadConst as u8,
                0,
                Opcode::LoadConst as u8,
                1,
                Opcode::LessThan as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
            result.chunk.constants,
            vec!(Value::Number(5.0), Value::Number(2.0))
        );
    }

    #[test]
    fn compile_less_than_equals_expr() {
        let result = run_test_compiler("5 <= 2");
        assert_eq!(
            result.chunk.code,
            vec!(
                Opcode::LoadConst as u8,
                0,
                Opcode::LoadConst as u8,
                1,
                Opcode::LessThanEquals as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
            result.chunk.constants,
            vec!(Value::Number(5.0), Value::Number(2.0))
        );
    }

    #[test]
    fn compile_greater_than_expr() {
        let result = run_test_compiler("5 > 2");
        assert_eq!(
            result.chunk.code,
            vec!(
                Opcode::LoadConst as u8,
                0,
                Opcode::LoadConst as u8,
                1,
                Opcode::GreaterThan as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
            result.chunk.constants,
            vec!(Value::Number(5.0), Value::Number(2.0),)
        );
    }

    #[test]
    fn compile_greater_than_equal_expr() {
        let result = run_test_compiler("5 >= 2");
        assert_eq!(
            result.chunk.code,
            vec!(
                Opcode::LoadConst as u8,
                0,
                Opcode::LoadConst as u8,
                1,
                Opcode::GreaterThanEquals as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
            result.chunk.constants,
            vec!(Value::Number(5.0), Value::Number(2.0),)
        );
    }

    #[test]
    fn compile_equals_to_expr() {
        let result = run_test_compiler("5 == 2");
        assert_eq!(
            result.chunk.code,
            vec!(
                Opcode::LoadConst as u8,
                0,
                Opcode::LoadConst as u8,
                1,
                Opcode::EqualsTo as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
            result.chunk.constants,
            vec!(Value::Number(5.0), Value::Number(2.0),)
        );
    }

    #[test]
    fn compile_not_equal_expr() {
        let result = run_test_compiler("5 != 2");
        assert_eq!(
            result.chunk.code,
            vec!(
                Opcode::LoadConst as u8,
                0,
                Opcode::LoadConst as u8,
                1,
                Opcode::NotEqual as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
            result.chunk.constants,
            vec!(Value::Number(5.0), Value::Number(2.0))
        );
    }

    #[test]
    fn compile_multiple_binary_expr() {
        let result = run_test_compiler("1 + 23 * 5");
        assert_eq!(
            result.chunk.code,
            vec!(
                Opcode::LoadConst as u8,
                0,
                Opcode::LoadConst as u8,
                1,
                Opcode::LoadConst as u8,
                2,
                Opcode::Multiply as u8,
                Opcode::Add as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(
            result.chunk.constants,
            vec!(Value::Number(1.0), Value::Number(23.0), Value::Number(5.0),)
        )
    }

    #[test]
    fn compile_unary_expr() {
        let result = run_test_compiler("-23");
        assert_eq!(
            result.chunk.code,
            vec!(
                Opcode::LoadConst as u8,
                0,
                Opcode::Negate as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            )
        );
        assert_eq!(result.chunk.constants, vec!(Value::Number(23.0)))
    }

    #[test]
    fn compile_unary_not() {
        let result = run_test_compiler("!true");
        assert_eq!(
            result.chunk.code,
            vec![
                Opcode::True as u8,
                Opcode::Not as u8,
                Opcode::Pop as u8,
                Opcode::Halt as u8
            ]
        );
    }

    #[test]
    fn compile_boolean_literal() {
        let result = run_test_compiler("true");
        assert_eq!(
            result.chunk.code,
            vec!(Opcode::True as u8, Opcode::Pop as u8, Opcode::Halt as u8)
        );

        let result = run_test_compiler("false");
        assert_eq!(
            result.chunk.code,
            vec!(Opcode::False as u8, Opcode::Pop as u8, Opcode::Halt as u8)
        );
    }

    #[test]
    fn compile_nil_literal() {
        let result = run_test_compiler("nil");
        assert_eq!(
            result.chunk.code,
            vec![Opcode::Nil as u8, Opcode::Pop as u8, Opcode::Halt as u8]
        );
    }

    #[test]
    fn compile_variable_declaration() {
        let result = run_test_compiler("var a");
        assert_eq!(
            result.chunk.code,
            vec![
                Opcode::Nil as u8,
                Opcode::DefGlobal as u8,
                0,
                0,
                0,
                0,
                Opcode::Halt as u8,
            ]
        )
    }

    #[test]
    fn compile_variable_declaration_with_value() {
        let result = run_test_compiler("var a = 23");
        assert_eq!(
            result.chunk.code,
            vec![
                Opcode::LoadConst as u8,
                1,
                Opcode::DefGlobal as u8,
                0,
                0,
                0,
                0,
                Opcode::Halt as u8,
            ]
        )
    }

    #[test]
    fn compile_print_statement() {
        let result = run_test_compiler("print 23");
        assert_eq!(
            result.chunk.code,
            vec![
                Opcode::LoadConst as u8,
                0,
                Opcode::Print as u8,
                Opcode::Halt as u8,
            ]
        )
    }

    #[test]
    fn compile_string_literal() {
        let result = run_test_compiler("\"Hello, World!\"");
        assert_eq!(
            result.chunk.code,
            vec![
                Opcode::LoadConst as u8,
                0,
                Opcode::Pop as u8,
                Opcode::Halt as u8,
            ]
        );
    }
}
*/
