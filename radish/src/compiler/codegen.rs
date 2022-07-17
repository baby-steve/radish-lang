use crate::common::{Chunk, CompiledModule, Disassembler, Module, Opcode, Span};

use crate::common::{Function as FunctionValue, Value};

use crate::compiler::{ast::*, Rc, SyntaxError};

use super::hoist::VarScope;
use super::pipeline::PipelineSettings;

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

#[derive(Debug, Copy, Clone, PartialEq)]
enum CompilerTarget {
    Function,
    Script,
    Constructor,
}

/// Track the state of the current `[Function]` being compiled.
#[derive(Debug)]
#[allow(unused)]
struct Frame {
    /// Function being written to.
    pub function: FunctionValue,
    /// The function's type
    pub function_type: CompilerTarget,
}

impl Frame {
    /// Helper method for creating a frame with a function type.
    pub fn function(fun: FunctionValue) -> Frame {
        Self::new(fun, CompilerTarget::Function)
    }

    /// Helper method for creating a frame with a script type.
    pub fn script(script: FunctionValue) -> Frame {
        Self::new(script, CompilerTarget::Script)
    }

    /// Helper method for creating a frame with a constructor type
    pub fn constructor(con: FunctionValue) -> Frame {
        Self::new(con, CompilerTarget::Constructor)
    }

    pub fn new(val: FunctionValue, typ: CompilerTarget) -> Frame {
        Self {
            function: val,
            function_type: typ,
        }
    }
}

struct CompilerSettings {
    pub dump_bytecode: bool,
    pub eval: bool,
}

impl CompilerSettings {
    pub fn new() -> Self {
        Self {
            dump_bytecode: false,
            eval: false,
        }
    }
}

impl Default for CompilerSettings {
    fn default() -> Self {
        Self::new()
    }
}

impl From<&PipelineSettings> for CompilerSettings {
    fn from(pipeline: &PipelineSettings) -> Self {
        Self {
            dump_bytecode: pipeline.dump_bytecode,
            eval: pipeline.repl,
        }
    }
}

pub struct Compiler {
    config: CompilerSettings,
    scope_depth: usize,
    frame_count: usize,
    frame: Vec<Frame>,
    loops: Vec<Loop>,
    module: CompiledModule,
    has_result: bool,
}

impl Compiler {
    pub fn new(config: &PipelineSettings) -> Self {
        let config = CompilerSettings::from(config);

        Self {
            config,
            scope_depth: 0,
            loops: vec![],
            frame_count: 0,
            frame: vec![],
            module: Module::empty(),
            has_result: false,
        }
    }

    // TODO: mutable borrow the ast.
    pub fn compile(&mut self, file_name: &str, ast: &AST) -> Result<CompiledModule, SyntaxError> {
        self.module.borrow_mut().name = file_name.to_string().into_boxed_str();

        let script = FunctionValue {
            arity: 0,
            chunk: Chunk::default(),
            name: String::from("").into_boxed_str(),
            module: Rc::downgrade(&self.module),
        };

        let frame = Frame::script(script);

        self.frame.push(frame);

        self.declare_globals(ast)?;

        for node in &ast.items {
            match self.statement(node) {
                Ok(_) => continue,
                Err(_) => continue,
            }
        }

        if self.config.eval && self.has_result {
            self.has_result = false;
            self.emit_byte(Opcode::Return as u8);
        } else {
            self.emit_return();
        }

        let script = self.frame.pop().unwrap().function;

        if self.config.dump_bytecode {
            Disassembler::disassemble_chunk(&file_name, &script);
        }

        self.module.borrow_mut().add_entry(script);

        Ok(Rc::clone(&self.module))
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
    fn define_variable(&mut self, id: &Ident) {
        //println!("[compiler] defining variable {}", &id.name);
        //println!("[compiler] scope: {:?}", &id.scope);
        if self.scope_depth == 0 {
            let index = self.module.borrow_mut().get_index(&id.name).unwrap();
            self.define_global(index as u32);
        } else if id.scope == VarScope::Local(true) {
            self.capture_local(id);
        }
    }

    /// Define a global variable.
    fn define_global(&mut self, global: u32) {
        self.emit_byte(Opcode::DefGlobal as u8);

        for byte in global.to_le_bytes() {
            self.emit_byte(byte);
        }
    }

    fn capture_local(&mut self, _id: &Ident) {
        //println!("[compiler] compiling captured local {}", _id.name);
        self.emit_byte(Opcode::DefCapture as u8);
    }

    /// Emit an `[Opcode]` to load the variable with the given name.
    fn load_variable(&mut self, id: &Ident) {
        use super::hoist::VarScope::*;

        match &id.scope {
            Local(_) => {
                self.emit_byte(Opcode::LoadLocal as u8);
                for byte in id.index.to_le_bytes() {
                    self.emit_byte(byte);
                }
            }
            NonLocal => {
                //println!("[compiler] compiling nonlocal with an index of {}", &id.index);
                self.emit_byte(Opcode::LoadCapture as u8);
                for byte in id.index.to_le_bytes() {
                    self.emit_byte(byte);
                }
            }
            Global => {
                self.emit_byte(Opcode::LoadGlobal as u8);
                let index: u32 = match self.module.borrow().get_index(&id.name) {
                    Some(i) => i as u32,
                    None => {
                        panic!("Module does not have a variable named '{}'", &id.name);
                    }
                };
                for byte in index.to_le_bytes() {
                    self.emit_byte(byte);
                }
            }
        }
    }

    /// Emit an `[Opcode]` to save a value to the variable with the
    /// given name.
    fn save_variable(&mut self, id: &Ident) {
        use super::hoist::VarScope::*;

        match &id.scope {
            Local(_) => {
                self.emit_byte(Opcode::SaveLocal as u8);
                for byte in id.index.to_le_bytes() {
                    self.emit_byte(byte);
                }
            }
            NonLocal => {
                //todo!("Haven't finished implementing upvalues");
                self.emit_byte(Opcode::SaveCapture as u8);
                for byte in id.index.to_le_bytes() {
                    self.emit_byte(byte);
                }
            }
            Global => {
                self.emit_byte(Opcode::SaveGlobal as u8);
                let index = self.module.borrow().get_index(&id.name).unwrap() as u32;
                for byte in index.to_le_bytes() {
                    self.emit_byte(byte);
                }
            }
        }

        // TODO: figure out why we need to emit an op delete. I seem to have forgotten...
        self.emit_byte(Opcode::Del as u8);
    }

    /// Enter a new block level scope.
    fn enter_scope(&mut self) {
        self.scope_depth += 1
    }

    /// Leave the current scope, removing all locally declared variables.
    fn leave_scope(&mut self) {
        self.scope_depth -= 1;
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

        if self.frame.last().unwrap().function_type != CompilerTarget::Constructor {
            self.emit_return();
        }

        self.frame_count -= 1;
        self.frame.pop().unwrap()
    }

    /// Sort of foward declare all globally scoped functions.
    fn declare_globals(&mut self, ast: &AST) -> Result<(), SyntaxError> {
        // add all global declarations to the module with a value of nil.
        for (name, _) in ast.scope.locals.iter() {
            self.module.borrow_mut().add_symbol(name.clone());
        }

        // next, go through and compile all global functions and classes.
        for node in &ast.items {
            match node {
                Stmt::FunDeclaration(fun, _) => {
                    // get the function's location in the module's variables array.
                    let index = self.module.borrow_mut().get_index(&fun.id.name).unwrap();
                    // compile the functions body.
                    self.function(fun)?;
                    // set the location in the module's variable array to the function's body.
                    self.define_global(index as u32);
                }
                Stmt::ClassDeclaration(class, _) => {
                    // get the class's location in the module's variables array.
                    let index = self.module.borrow_mut().get_index(&class.id.name).unwrap();
                    // compile the class body.
                    self.class(class)?;
                    // define the class in the global scope.
                    self.define_global(index as u32);
                }
                Stmt::ImportStmt(import) => {
                    self.module
                        .borrow_mut()
                        .add_symbol(import.name().unwrap().name.clone());
                }
                _ => continue,
            }
        }

        Ok(())
    }

    /// Compile a function declaration,
    fn function(&mut self, fun: &FunctionDecl) -> Result<(), SyntaxError> {
        //println!("{:#?}", fun);

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
                self.define_variable(&param);
            }

            for stmt in fun.body.iter() {
                self.statement(stmt)?;
            }
        }

        let frame = self.leave_function();

        if self.config.dump_bytecode {
            Disassembler::disassemble_chunk(&frame.function.name, &frame.function);
        }

        self.emit_constant(Value::from(frame.function));
        self.emit_byte(Opcode::Closure as u8);

        let scope = fun.other_scope.clone().unwrap();

        if scope.upvalues.len() > u8::MAX.into() {
            panic!("Too many upvalues");
        };

        self.emit_byte(scope.upvalues.len() as u8);

        for upval in scope.upvalues.iter() {
            // TODO: replace the 1 and 0 with globals or something for readablity.
            if upval.on_stack {
                self.emit_byte(0);
            } else {
                self.emit_byte(1);
            }

            self.emit_byte(upval.index as u8);
        }

        Ok(())
    }

    /// Compile a class declaration
    fn class(&mut self, class: &ClassDecl) -> Result<(), SyntaxError> {
        for method in class.methods.iter() {
            self.function(method)?;
        }

        for constructor in class.constructors.iter() {
            self.constructor_declaration(constructor, &class.id)?;
        }

        for field in class.fields.iter() {
            // access type.
            // self.emit_byte(1 as u8);
            // the name of the field.
            self.emit_constant(Value::from(&field.name.name));
            // the field's value
            if let Some(expr) = &field.init {
                self.expression(expr)?;
            } else {
                self.emit_byte(Opcode::Nil as u8);
            }
        }

        // the name of the class
        self.emit_constant(Value::from(&class.id.name));

        self.emit_byte(Opcode::BuildClass as u8);

        // number of fields
        self.emit_byte(class.fields.len() as u8);
        // number of constructors
        self.emit_byte(class.constructors.len() as u8);
        // number of methods
        self.emit_byte(class.methods.len() as u8);

        Ok(())
    }
}

impl Compiler {
    fn statement(&mut self, stmt: &Stmt) -> Result<(), SyntaxError> {
        match stmt {
            Stmt::BlockStmt(body, _) => self.block(body),
            Stmt::ExpressionStmt(expr) => self.expression_stmt(expr),
            Stmt::FunDeclaration(fun, _) => self.function_declaration(fun),
            Stmt::ClassDeclaration(class, _) => self.class_declaration(class),
            Stmt::VarDeclaration(stmt, _) => self.var_declaration(stmt),
            Stmt::AssignmentStmt(stmt, _) => self.assignment(stmt),
            Stmt::IfStmt(expr, body, alt, _) => self.if_statement(expr, body, alt),
            Stmt::LoopStmt(body, _) => self.loop_statement(body),
            Stmt::WhileStmt(expr, body, _) => self.while_statement(expr, body),
            Stmt::ImportStmt(stmt) => self.import_statement(stmt),
            Stmt::ReturnStmt(value, _) => self.return_statement(value),
            Stmt::BreakStmt(pos) => self.break_statement(pos),
            Stmt::ContinueStmt(pos) => self.continue_statement(pos),
            Stmt::PrintStmt(expr, _) => self.print(expr),
            _ => unreachable!(),
            // Stmt::ConDeclaration(con, _) => self.constructor_declaration(con),
        }
    }

    fn function_declaration(&mut self, fun: &FunctionDecl) -> Result<(), SyntaxError> {
        // at this point all global functions have already been compiled,
        // so only locally scoped functions have to be handled.

        if self.scope_depth > 0 {
            self.function(fun)?;
            self.define_variable(&fun.id);
        }

        Ok(())
    }

    fn class_declaration(&mut self, class: &ClassDecl) -> Result<(), SyntaxError> {
        // at this point all global classes have already been compiled,
        // so only locally scoped classes have to be handled.
        if self.scope_depth > 0 {
            self.class(class)?;
            self.define_variable(&class.id);
        }

        Ok(())
    }

    fn constructor_declaration(
        &mut self,
        con: &ConstructorDecl,
        class: &Ident,
    ) -> Result<(), SyntaxError> {
        // the constructor body
        // FIXME: a lot of duplicate code between constructors and functions.
        if con.params.len() > u8::MAX.into() {
            panic!("Cannot have more than 255 parameters");
        }

        let frame = FunctionValue {
            arity: con.params.len() as u8,
            name: con.id.name.clone().into_boxed_str(),
            chunk: Chunk::default(),
            module: Rc::downgrade(&self.module),
        };

        self.enter_function(Frame::constructor(frame));
        {
            self.load_variable(class);
            self.emit_byte(Opcode::Construct as u8);

            for param in &con.params {
                self.define_variable(&param);
            }

            for stmt in con.body.iter() {
                self.statement(stmt)?;
            }

            self.emit_byte(Opcode::LoadLocal0 as u8);
            self.emit_byte(Opcode::Return as u8);
        }

        let frame = self.leave_function();

        if self.config.dump_bytecode {
            Disassembler::disassemble_chunk(&frame.function.name, &frame.function);
        }

        // emit the constructor's body
        self.emit_constant(Value::from(frame.function));

        // create a closure from the constructor's body
        self.emit_byte(Opcode::Closure as u8);

        // the number of variable's the closure captures.
        self.emit_byte(0);

        // build the constructor
        // self.emit_byte(Opcode::BuildCon as u8);

        Ok(())
    }

    fn if_statement(
        &mut self,
        expr: &Expr,
        body: &[Stmt],
        else_branch: &Option<Box<Stmt>>,
    ) -> Result<(), SyntaxError> {
        self.expression(expr)?;
        let then_jump = self.emit_jump(Opcode::JumpIfFalse);
        self.emit_byte(Opcode::Del as u8);
        self.block(body)?;

        let else_jump = self.emit_jump(Opcode::Jump);

        self.patch_jump(then_jump);
        self.emit_byte(Opcode::Del as u8);

        if let Some(else_branch) = &else_branch {
            self.statement(else_branch)?;
        }

        self.patch_jump(else_jump);

        Ok(())
    }

    fn loop_statement(&mut self, body: &[Stmt]) -> Result<(), SyntaxError> {
        let loop_start = self.last_byte();
        self.enter_loop(loop_start);

        self.block(body)?;
        self.emit_loop(loop_start);

        self.leave_loop();

        Ok(())
    }

    fn while_statement(&mut self, expr: &Expr, body: &[Stmt]) -> Result<(), SyntaxError> {
        let loop_start = self.last_byte();
        self.enter_loop(loop_start);

        self.expression(expr)?;
        let exit_jump = self.emit_jump(Opcode::JumpIfFalse);
        self.emit_byte(Opcode::Del as u8);

        self.block(body)?;

        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.leave_loop();
        self.emit_byte(Opcode::Del as u8);

        Ok(())
    }

    fn block(&mut self, body: &[Stmt]) -> Result<(), SyntaxError> {
        self.enter_scope();

        for node in body {
            self.statement(node)?;
        }

        self.leave_scope();

        for stmt in body {
            match stmt {
                Stmt::VarDeclaration(_, _) => {
                    self.emit_byte(Opcode::Del as u8);
                }
                _ => continue,
            }
        }

        Ok(())
    }

    fn import_statement(&mut self, import_stmt: &ImportStatement) -> Result<(), SyntaxError> {
        self.string(import_stmt.path())?;

        self.emit_byte(Opcode::Import as u8);

        self.define_variable(&import_stmt.name().unwrap());

        Ok(())
    }

    fn return_statement(&mut self, return_val: &Option<Expr>) -> Result<(), SyntaxError> {
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

    fn break_statement(&mut self, _: &Span) -> Result<(), SyntaxError> {
        let exit_jump = self.emit_jump(Opcode::Jump);
        self.emit_byte(Opcode::Del as u8);

        let index = &self.loops.len() - 1;
        self.loops[index].jump_placeholders.push(exit_jump);

        Ok(())
    }

    fn continue_statement(&mut self, _: &Span) -> Result<(), SyntaxError> {
        let loop_start = self.loops.last().unwrap().loop_start;

        self.emit_loop(loop_start);

        Ok(())
    }

    fn print(&mut self, expr: &Expr) -> Result<(), SyntaxError> {
        self.expression(expr)?;

        self.emit_byte(Opcode::Print as u8);

        Ok(())
    }

    fn var_declaration(&mut self, stmt: &VarDeclaration) -> Result<(), SyntaxError> {
        if self.scope_depth > 0 {
            if let Some(expr) = &stmt.init {
                self.expression(expr)?;
            } else {
                self.emit_byte(Opcode::Nil as u8);
            }

            self.define_variable(&stmt.name);
        } else {
            // All global variables have already been foward declared, so we can just
            // grab its location from the module.
            let index = self.module.borrow_mut().get_index(&stmt.name.name).unwrap();

            if let Some(expr) = &stmt.init {
                self.expression(expr)?;
            } else {
                self.emit_byte(Opcode::Nil as u8);
            }

            self.define_global(index as u32);
        }

        Ok(())
    }

    fn assignment(&mut self, stmt: &AssignmentStmt) -> Result<(), SyntaxError> {
        let op = match stmt.op {
            OpAssignment::AddAssign => Some(Opcode::Add),
            OpAssignment::SubAssign => Some(Opcode::Sub),
            OpAssignment::MulAssign => Some(Opcode::Mul),
            OpAssignment::DivAssign => Some(Opcode::Div),
            OpAssignment::RemAssign => Some(Opcode::Rem),
            OpAssignment::Equals => None,
        };

        match &stmt.lhs {
            Expr::Identifier(id) => {
                if let Some(op) = op {
                    self.load_variable(&id);
                    self.expression(&stmt.rhs)?;
                    self.emit_byte(op as u8);
                } else {
                    self.expression(&stmt.rhs)?;
                }

                self.save_variable(id);
            }
            Expr::MemberExpr(obj, prop, _) => {
                self.expression(obj)?;

                match &**prop {
                    Expr::MemberExpr(object, property, _) => {
                        self.member_expr(&object, &property)?
                    }
                    Expr::Identifier(id) => self.emit_constant(Value::from(&id.name)),
                    prop => self.expression(prop)?,
                };

                if let Some(op) = op {
                    self.expression(obj)?;
                    self.expression(prop)?;
                    self.emit_byte(Opcode::LoadField as u8);

                    self.expression(&stmt.rhs)?;
                    self.emit_byte(op as u8);
                } else {
                    self.expression(&stmt.rhs)?;
                }

                self.emit_byte(Opcode::SaveField as u8);
            }
            _ => unreachable!("Invalid left hand side of assignment statement"),
        }

        Ok(())
    }

    fn expression_stmt(&mut self, expr: &Expr) -> Result<(), SyntaxError> {
        self.expression(expr)?;

        if self.config.eval && self.frame_count == 0 {
            self.emit_byte(Opcode::NoOp as u8);
            self.has_result = true;
        } else {
            self.emit_byte(Opcode::Del as u8);
        }

        Ok(())
    }

    fn expression(&mut self, expr: &Expr) -> Result<(), SyntaxError> {
        match expr {
            Expr::ArrayExpr(array, _) => self.array(array),
            Expr::MapExpr(values, _) => self.map(values),
            Expr::BinaryExpr(expr, _) => self.binary_expression(expr),
            Expr::ParenExpr(expr, _) => self.expression(expr),
            Expr::UnaryExpr(op, arg, _) => self.unary(arg, op),
            Expr::LogicalExpr(expr, _) => self.logical_expr(expr),
            Expr::CallExpr(callee, args, _) => self.call_expr(callee, args),
            Expr::MemberExpr(obj, prop, _) => self.member_expr(obj, prop),
            Expr::Identifier(id) => self.identifier(id),
            Expr::This(_) => self.this(),
            Expr::Number(val, _) => self.number(val),
            Expr::String(val, _) => self.string(val),
            Expr::Bool(val, _) => self.boolean(val),
            Expr::Nil(_) => self.nil(),
        }
    }

    fn array(&mut self, array: &[Expr]) -> Result<(), SyntaxError> {
        for element in array.iter().rev() {
            self.expression(element)?;
        }

        self.emit_byte(Opcode::BuildArray as u8);

        let element_count = array.len() as u32;

        for byte in element_count.to_le_bytes() {
            self.emit_byte(byte);
        }

        Ok(())
    }

    fn map(&mut self, values: &[Expr]) -> Result<(), SyntaxError> {
        for pair in values.chunks(2).rev() {
            self.expression(&pair[0])?;
            self.expression(&pair[1])?;
        }

        self.emit_byte(Opcode::BuildMap as u8);

        let element_count = values.len() as u32 / 2;

        for byte in element_count.to_le_bytes() {
            self.emit_byte(byte);
        }

        Ok(())
    }

    fn binary_expression(&mut self, expr: &BinaryExpr) -> Result<(), SyntaxError> {
        self.expression(&expr.lhs)?;
        self.expression(&expr.rhs)?;

        match &expr.op {
            Op::Add => self.emit_byte(Opcode::Add as u8),
            Op::Subtract => self.emit_byte(Opcode::Sub as u8),
            Op::Multiply => self.emit_byte(Opcode::Mul as u8),
            Op::Divide => self.emit_byte(Opcode::Div as u8),
            Op::Remainder => self.emit_byte(Opcode::Rem as u8),
            Op::LessThan => self.emit_byte(Opcode::CmpLT as u8),
            Op::LessThanEquals => self.emit_byte(Opcode::CmpLTEq as u8),
            Op::GreaterThan => self.emit_byte(Opcode::CmpGT as u8),
            Op::GreaterThanEquals => self.emit_byte(Opcode::CmpGTEq as u8),
            Op::EqualsTo => self.emit_byte(Opcode::CmpEq as u8),
            Op::NotEqual => self.emit_byte(Opcode::CmpNotEq as u8),
            _ => unreachable!("{:?} is not a binary operator.", &expr.op),
        }

        Ok(())
    }

    fn logical_expr(&mut self, expr: &BinaryExpr) -> Result<(), SyntaxError> {
        self.expression(&expr.lhs)?;

        let op = match &expr.op {
            Op::And => Opcode::JumpIfFalse,
            Op::Or => Opcode::JumpIfTrue,
            _ => unreachable!("Invalid logical operator."),
        };

        let end_jump = self.emit_jump(op);
        self.emit_byte(Opcode::Del as u8);

        self.expression(&expr.rhs)?;

        self.patch_jump(end_jump);

        Ok(())
    }

    fn unary(&mut self, arg: &Expr, op: &Op) -> Result<(), SyntaxError> {
        self.expression(arg)?;

        match op {
            Op::Subtract => self.emit_byte(Opcode::Neg as u8),
            Op::Bang => self.emit_byte(Opcode::Not as u8),
            _ => unreachable!("{:?} is not an unary operator.", &op),
        }

        Ok(())
    }

    fn call_expr(&mut self, callee: &Expr, args: &[Expr]) -> Result<(), SyntaxError> {
        if args.len() > u8::MAX.into() {
            panic!("Cannot have more than 255 arguments.");
        }

        self.expression(callee)?;

        for arg in args.iter() {
            self.expression(arg)?;
        }

        self.emit_bytes(Opcode::Call as u8, args.len() as u8);

        Ok(())
    }

    fn member_expr(&mut self, object: &Expr, property: &Expr) -> Result<(), SyntaxError> {
        match property {
            Expr::MemberExpr(object, property, _) => self.member_expr(object, property)?,
            Expr::Identifier(id) => self.emit_constant(Value::from(&id.name)),
            _ => panic!("Invalid property in member expression"),
        };

        self.expression(object)?;

        self.emit_byte(Opcode::LoadField as u8);

        Ok(())
    }

    fn this(&mut self) -> Result<(), SyntaxError> {
        self.emit_byte(Opcode::LoadLocal0 as u8);

        Ok(())
    }

    fn identifier(&mut self, id: &Ident) -> Result<(), SyntaxError> {
        self.load_variable(&id);
        Ok(())
    }

    fn number(&mut self, val: &f64) -> Result<(), SyntaxError> {
        self.emit_constant(Value::Number(*val));
        Ok(())
    }

    fn string(&mut self, val: &str) -> Result<(), SyntaxError> {
        self.emit_constant(Value::from(val));
        Ok(())
    }

    fn boolean(&mut self, val: &bool) -> Result<(), SyntaxError> {
        match val {
            true => self.emit_byte(Opcode::True as u8),
            false => self.emit_byte(Opcode::False as u8),
        }
        Ok(())
    }

    fn nil(&mut self) -> Result<(), SyntaxError> {
        self.emit_byte(Opcode::Nil as u8);
        Ok(())
    }
}
