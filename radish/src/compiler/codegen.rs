use crate::common::{Chunk, CompiledModule, Disassembler, Module, Opcode};

use crate::common::{Function as FunctionValue, Value};

use crate::compiler::{ast::*, Rc, SyntaxError};

use super::ast_ast::{
    ArrayExpr, AssignmentExpr, BinaryExpr, BinaryOp, BlockStmt, BreakStmt, CallExpr, ClassDecl,
    ContinueStmt, ExpressionStmt, FunctionDecl, Identifier, IfStmt, ImportStmt, Literal,
    LiteralKind, LogicalExpr, LogicalOp, LoopStmt, MapExpr, MemberExpr, MethodDecl, MethodKind,
    OpAssignment, ReturnStmt, UnaryExpr, UnaryOp, VariableDecl, WhileStmt,
};
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
    fn define_variable(&mut self, id: &Identifier) {
        if self.scope_depth == 0 {
            let index = self.module.borrow_mut().get_index(&id.name).unwrap();
            self.define_global(index as u32);
        } else if id.typ == VarScope::Local(true) {
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

    fn capture_local(&mut self, _id: &Identifier) {
        //println!("[compiler] compiling captured local {}", _id.name);
        self.emit_byte(Opcode::DefCapture as u8);
    }

    /// Emit an `[Opcode]` to load the variable with the given name.
    fn load_variable(&mut self, id: &Identifier) {
        use super::hoist::VarScope::*;

        match &id.typ {
            Local(_) => {
                self.emit_byte(Opcode::LoadLocal as u8);
                for byte in id.index.to_le_bytes() {
                    self.emit_byte(byte);
                }
            }
            NonLocal => {
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
            Unknown => unreachable!("Identifier scope must be known by compile time"),
        }
    }

    /// Emit an `[Opcode]` to save a value to the variable with the
    /// given name.
    fn save_variable(&mut self, id: &Identifier) {
        use super::hoist::VarScope::*;

        match &id.typ {
            Local(_) => {
                self.emit_byte(Opcode::SaveLocal as u8);
                for byte in id.index.to_le_bytes() {
                    self.emit_byte(byte);
                }
            }
            NonLocal => {
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
            Unknown => unreachable!("Identifier scope must be known by compile time"),
        }

        // self.emit_byte(Opcode::Del as u8);
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
                Stmt::FunctionDecl(fun) => {
                    // get the function's location in the module's variables array.
                    let index = self.module.borrow_mut().get_index(&fun.id.name).unwrap();
                    // compile the functions body.
                    self.function(fun)?;
                    // set the location in the module's variable array to the function's body.
                    self.define_global(index as u32);
                }
                Stmt::ClassDecl(class) => {
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
        let mut compiler = FunctionCompiler::new(self);

        compiler.compile(fun);

        Ok(())
    }

    /// Compile a class declaration
    fn class(&mut self, class: &ClassDecl) -> Result<(), SyntaxError> {
        // for method in class.methods.iter() {
        //     self.function(method)?;
        // }

        // for constructor in class.constructors.iter() {
        //     self.constructor_declaration(constructor, &class.id)?;
        // }

        // for field in class.fields.iter() {
        //     // access type.
        //     // self.emit_byte(1 as u8);
        //     // the name of the field.
        //     self.emit_constant(Value::from(&field.id.name));
        //     // the field's value
        //     if let Some(expr) = &field.init {
        //         self.expression(expr)?;
        //     } else {
        //         self.emit_byte(Opcode::Nil as u8);
        //     }
        // }

        // the name of the class
        self.emit_constant(Value::from(&class.id.name));

        self.emit_byte(Opcode::BuildClass as u8);

        // number of fields
        // self.emit_byte(class.fields.len() as u8);
        // number of constructors
        // self.emit_byte(class.constructors.len() as u8);
        // number of methods
        // self.emit_byte(class.methods.len() as u8);

        Ok(())
    }
}

impl Compiler {
    fn statement(&mut self, stmt: &Stmt) -> Result<(), SyntaxError> {
        match stmt {
            Stmt::BlockStmt(block_stmt) => self.block(block_stmt),
            Stmt::ExpressionStmt(expr) => self.expression_stmt(expr),
            Stmt::FunctionDecl(fun) => self.function_declaration(fun),
            Stmt::MethodDecl(method) => self.method_declaration(method),
            Stmt::ClassDecl(class) => self.class_declaration(class),
            Stmt::VariableDecl(stmt) => self.var_declaration(stmt),
            Stmt::IfStmt(stmt) => self.if_statement(stmt),
            Stmt::LoopStmt(stmt) => self.loop_statement(stmt),
            Stmt::WhileStmt(stmt) => self.while_statement(stmt),
            Stmt::ImportStmt(stmt) => self.import_statement(stmt),
            Stmt::ReturnStmt(stmt) => self.return_statement(stmt),
            Stmt::BreakStmt(stmt) => self.break_statement(stmt),
            Stmt::ContinueStmt(stmt) => self.continue_statement(stmt),
            Stmt::PrintStmt(expr, _) => self.print(expr),
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

    fn method_declaration(&mut self, method: &MethodDecl) -> Result<(), SyntaxError> {
        let mut _compiler = FunctionCompiler::new(self);

        if let MethodKind::Constructor = method.kind {
            // ...
        }

        todo!()
    }

    /*fn constructor_declaration(
        &mut self,
        con: &ConstructorDecl,
        class: &Identifier,
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
    }*/

    fn if_statement(&mut self, stmt: &IfStmt) -> Result<(), SyntaxError> {
        self.expression(&stmt.condition)?;
        let then_jump = self.emit_jump(Opcode::JumpIfFalse);
        self.emit_byte(Opcode::Del as u8);

        self.block(&stmt.consequent)?;

        let else_jump = self.emit_jump(Opcode::Jump);

        self.patch_jump(then_jump);
        self.emit_byte(Opcode::Del as u8);

        if let Some(alt) = &stmt.alternate {
            self.statement(alt)?;
        }

        self.patch_jump(else_jump);

        Ok(())
    }

    fn loop_statement(&mut self, stmt: &LoopStmt) -> Result<(), SyntaxError> {
        let loop_start = self.last_byte();
        self.enter_loop(loop_start);

        self.block(&stmt.body)?;

        self.emit_loop(loop_start);

        self.leave_loop();

        Ok(())
    }

    fn while_statement(&mut self, stmt: &WhileStmt) -> Result<(), SyntaxError> {
        let loop_start = self.last_byte();
        self.enter_loop(loop_start);

        self.expression(&stmt.condition)?;
        let exit_jump = self.emit_jump(Opcode::JumpIfFalse);
        self.emit_byte(Opcode::Del as u8);

        self.block(&stmt.body)?;

        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.leave_loop();
        self.emit_byte(Opcode::Del as u8);

        Ok(())
    }

    fn block(&mut self, block_stmt: &BlockStmt) -> Result<(), SyntaxError> {
        self.enter_scope();

        for node in block_stmt.body.iter() {
            self.statement(node)?;
        }

        self.leave_scope();

        for stmt in block_stmt.body.iter() {
            match stmt {
                Stmt::VariableDecl(_) => {
                    self.emit_byte(Opcode::Del as u8);
                }
                _ => continue,
            }
        }

        Ok(())
    }

    fn import_statement(&mut self, import_stmt: &ImportStmt) -> Result<(), SyntaxError> {
        self.string(&import_stmt.source)?;

        self.emit_byte(Opcode::Import as u8);

        self.define_variable(&import_stmt.name().unwrap());

        Ok(())
    }

    fn return_statement(&mut self, return_stmt: &ReturnStmt) -> Result<(), SyntaxError> {
        if self.scope_depth == 0 {
            panic!("Return statement outside of a function");
        }

        if let Some(expr) = &return_stmt.argument {
            self.expression(expr)?;
        } else {
            self.emit_byte(Opcode::Nil as u8);
        }

        self.emit_byte(Opcode::Return as u8);

        Ok(())
    }

    fn break_statement(&mut self, _: &BreakStmt) -> Result<(), SyntaxError> {
        let exit_jump = self.emit_jump(Opcode::Jump);
        self.emit_byte(Opcode::Del as u8);

        let index = &self.loops.len() - 1;
        self.loops[index].jump_placeholders.push(exit_jump);

        Ok(())
    }

    fn continue_statement(&mut self, _: &ContinueStmt) -> Result<(), SyntaxError> {
        let loop_start = self.loops.last().unwrap().loop_start;

        self.emit_loop(loop_start);

        Ok(())
    }

    fn print(&mut self, expr: &Expr) -> Result<(), SyntaxError> {
        self.expression(expr)?;

        self.emit_byte(Opcode::Print as u8);

        Ok(())
    }

    fn var_declaration(&mut self, stmt: &VariableDecl) -> Result<(), SyntaxError> {
        if self.scope_depth > 0 {
            if let Some(expr) = &stmt.init {
                self.expression(expr)?;
            } else {
                self.emit_byte(Opcode::Nil as u8);
            }

            self.define_variable(&stmt.id);
        } else {
            // All global variables have already been foward declared, so we can just
            // grab its location from the module.
            let index = self.module.borrow_mut().get_index(&stmt.id.name).unwrap();

            if let Some(expr) = &stmt.init {
                self.expression(expr)?;
            } else {
                self.emit_byte(Opcode::Nil as u8);
            }

            self.define_global(index as u32);
        }

        Ok(())
    }

    fn assignment(&mut self, assignment_expr: &AssignmentExpr) -> Result<(), SyntaxError> {
        let op = match assignment_expr.op {
            OpAssignment::AddAssign => Some(Opcode::Add),
            OpAssignment::SubAssign => Some(Opcode::Sub),
            OpAssignment::MulAssign => Some(Opcode::Mul),
            OpAssignment::DivAssign => Some(Opcode::Div),
            OpAssignment::RemAssign => Some(Opcode::Rem),
            OpAssignment::Equals => None,
        };

        match &*assignment_expr.lhs {
            Expr::Identifier(id) => {
                if let Some(op) = op {
                    self.load_variable(&id);
                    self.expression(&assignment_expr.rhs)?;
                    self.emit_byte(op as u8);
                } else {
                    self.expression(&assignment_expr.rhs)?;
                }

                self.save_variable(id);
            }
            Expr::MemberExpr(expr) => {
                self.expression(&expr.object)?;

                match &**&expr.property {
                    Expr::MemberExpr(expr) => self.member_expr(expr)?,
                    Expr::Identifier(id) => self.emit_constant(Value::from(&id.name)),
                    prop => self.expression(prop)?,
                };

                if let Some(op) = op {
                    self.expression(&expr.object)?;
                    self.expression(&expr.property)?;
                    self.emit_byte(Opcode::LoadField as u8);

                    self.expression(&assignment_expr.rhs)?;
                    self.emit_byte(op as u8);
                } else {
                    self.expression(&assignment_expr.rhs)?;
                }

                self.emit_byte(Opcode::SaveField as u8);
            }
            _ => unreachable!("Invalid left hand side of assignment statement"),
        }

        Ok(())
    }

    fn expression_stmt(&mut self, stmt: &ExpressionStmt) -> Result<(), SyntaxError> {
        self.expression(&stmt.expr)?;

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
            Expr::ArrayExpr(array) => self.array(array),
            Expr::MapExpr(expr) => self.map(expr),
            Expr::AssignmentExpr(expr) => self.assignment(expr),
            Expr::BinaryExpr(expr) => self.binary_expression(expr),
            Expr::UnaryExpr(expr) => self.unary(expr),
            Expr::LogicalExpr(expr) => self.logical_expr(expr),
            Expr::CallExpr(expr) => self.call_expr(expr),
            Expr::MemberExpr(expr) => self.member_expr(expr),
            Expr::Identifier(id) => self.identifier(id),
            Expr::This(_) => self.this(),
            Expr::Literal(val) => self.literal(val),
        }
    }

    fn array(&mut self, array: &ArrayExpr) -> Result<(), SyntaxError> {
        for element in array.elements.iter().rev() {
            self.expression(element)?;
        }

        self.emit_byte(Opcode::BuildArray as u8);

        let element_count = array.elements.len() as u32;

        for byte in element_count.to_le_bytes() {
            self.emit_byte(byte);
        }

        Ok(())
    }

    fn map(&mut self, map_expr: &MapExpr) -> Result<(), SyntaxError> {
        for prop in map_expr.properties.iter() {
            self.expression(&prop.key)?;
            self.expression(&prop.value)?;
        }

        self.emit_byte(Opcode::BuildMap as u8);

        let element_count = map_expr.properties.len() as u32 / 2;

        for byte in element_count.to_le_bytes() {
            self.emit_byte(byte);
        }

        Ok(())
    }

    fn binary_expression(&mut self, expr: &BinaryExpr) -> Result<(), SyntaxError> {
        self.expression(&expr.lhs)?;
        self.expression(&expr.rhs)?;

        match &expr.op {
            BinaryOp::Add => self.emit_byte(Opcode::Add as u8),
            BinaryOp::Subtract => self.emit_byte(Opcode::Sub as u8),
            BinaryOp::Multiply => self.emit_byte(Opcode::Mul as u8),
            BinaryOp::Divide => self.emit_byte(Opcode::Div as u8),
            BinaryOp::Remainder => self.emit_byte(Opcode::Rem as u8),
            BinaryOp::LessThan => self.emit_byte(Opcode::CmpLT as u8),
            BinaryOp::LessThanEquals => self.emit_byte(Opcode::CmpLTEq as u8),
            BinaryOp::GreaterThan => self.emit_byte(Opcode::CmpGT as u8),
            BinaryOp::GreaterThanEquals => self.emit_byte(Opcode::CmpGTEq as u8),
            BinaryOp::EqualsTo => self.emit_byte(Opcode::CmpEq as u8),
            BinaryOp::NotEqual => self.emit_byte(Opcode::CmpNotEq as u8),
        }

        Ok(())
    }

    fn logical_expr(&mut self, expr: &LogicalExpr) -> Result<(), SyntaxError> {
        self.expression(&expr.lhs)?;

        let op = match &expr.op {
            LogicalOp::And => Opcode::JumpIfFalse,
            LogicalOp::Or => Opcode::JumpIfTrue,
        };

        let end_jump = self.emit_jump(op);
        self.emit_byte(Opcode::Del as u8);

        self.expression(&expr.rhs)?;

        self.patch_jump(end_jump);

        Ok(())
    }

    fn unary(&mut self, expr: &UnaryExpr) -> Result<(), SyntaxError> {
        self.expression(&expr.argument)?;

        match expr.op {
            UnaryOp::Minus => self.emit_byte(Opcode::Neg as u8),
            UnaryOp::Not => self.emit_byte(Opcode::Not as u8),
        }

        Ok(())
    }

    fn call_expr(&mut self, expr: &CallExpr) -> Result<(), SyntaxError> {
        if expr.args.len() > u8::MAX.into() {
            panic!("Cannot have more than 255 arguments.");
        }

        self.expression(&expr.callee)?;

        for arg in expr.args.iter() {
            self.expression(arg)?;
        }

        self.emit_bytes(Opcode::Call as u8, expr.args.len() as u8);

        Ok(())
    }

    fn member_expr(&mut self, expr: &MemberExpr) -> Result<(), SyntaxError> {
        match &*expr.property {
            Expr::MemberExpr(expr) => self.member_expr(&expr)?,
            Expr::Identifier(id) => self.emit_constant(Value::from(&id.name)),
            expr => self.expression(&expr)?,
        };

        self.expression(&expr.object)?;

        self.emit_byte(Opcode::LoadField as u8);

        Ok(())
    }

    fn this(&mut self) -> Result<(), SyntaxError> {
        self.emit_byte(Opcode::LoadLocal0 as u8);

        Ok(())
    }

    fn identifier(&mut self, id: &Identifier) -> Result<(), SyntaxError> {
        self.load_variable(&id);
        Ok(())
    }

    fn literal(&mut self, lit: &Literal) -> Result<(), SyntaxError> {
        match &lit.kind {
            LiteralKind::String(val) => self.string(val),
            LiteralKind::Number(val) => self.number(val),
            LiteralKind::Boolean(val) => self.boolean(val),
            LiteralKind::Nil => self.nil(),
        }
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

struct FunctionCompiler<'a> {
    start: Option<fn(&mut Compiler)>,
    end: Option<fn(&mut Compiler)>,
    compiler: &'a mut Compiler,
}

impl<'a> FunctionCompiler<'a> {
    pub fn new(compiler: &'a mut Compiler) -> Self {
        Self {
            start: None,
            end: None,
            compiler,
        }
    }

    // pub fn on_start(&mut self, f: fn(&mut Compiler)) {
    //     self.start = Some(f);
    // }

    // pub fn on_end(&mut self, f: fn(&mut Compiler)) {
    //     self.end = Some(f);
    // }

    pub fn compile(&mut self, fun: &FunctionDecl) {
        if fun.params.len() > u8::MAX.into() {
            panic!("Cannot have more than 255 parameters");
        }

        let frame = FunctionValue {
            arity: fun.params.len() as u8,
            name: fun.id.name.clone().into_boxed_str(),
            chunk: Chunk::default(),
            module: Rc::downgrade(&self.compiler.module),
        };

        self.compiler.enter_function(Frame::function(frame));
        {
            if let Some(f) = self.start {
                (f)(self.compiler);
            }

            for param in &fun.params {
                self.compiler.define_variable(&param);
            }

            self.compiler
                .block(&fun.body)
                .expect("couldn't compile function block");

            if let Some(f) = self.end {
                (f)(self.compiler);
            }
        }

        let frame = self.compiler.leave_function();

        // if self.config.dump_bytecode {
        // Disassembler::disassemble_chunk(&frame.function.name, &frame.function);
        // }

        self.compiler.emit_constant(Value::from(frame.function));
        self.compiler.emit_byte(Opcode::Closure as u8);

        let scope = fun.scope.clone().unwrap();

        if scope.upvalues.len() > u8::MAX.into() {
            panic!("Too many upvalues");
        };

        self.compiler.emit_byte(scope.upvalues.len() as u8);

        for upval in scope.upvalues.iter() {
            // TODO: replace the 1 and 0 with globals or something for readablity.
            if upval.on_stack {
                self.compiler.emit_byte(0);
            } else {
                self.compiler.emit_byte(1);
            }

            self.compiler.emit_byte(upval.index as u8);
        }
    }
}
