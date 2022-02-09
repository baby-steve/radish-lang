pub mod common;
pub mod compiler;
pub mod error;
pub mod vm;

use std::cell::RefCell;
use std::rc::Rc;

use common::{source::Source, value::Function, value::Module};
use compiler::{
    analysis::Analyzer, ast::AST, compiler::Compiler, parser::Parser, table::SymbolTable,
};
use vm::vm::VM;

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

#[derive(Debug, Clone, PartialEq)]
pub enum RadishError {
    CompilerError,
    RuntimeError,
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

/// The rudimentary wrapper of sorts for the Radish language.
pub struct Radish {
    config: Rc<RadishConfig>,
}

impl Radish {
    pub fn new() -> Radish {
        Radish::with_settings(RadishConfig::new())
    }

    pub fn with_settings(config: Rc<RadishConfig>) -> Radish {
        Radish {
            config,
        }
    }

    pub fn parse_source(&mut self, source: Rc<Source>) -> AST {
        let mut parser = Parser::new(source);
        match parser.parse() {
            Ok(ast) => ast,
            Err(_) => todo!(),
        }
    }

    pub fn check(&mut self, ast: &AST) -> SymbolTable {
        let mut analyzer = Analyzer::new();
        match analyzer.analyze(ast) {
            Ok(table) => table,
            Err(_) => todo!(),
        }
    }

    pub fn compile(&mut self, ast: &AST, scope: &SymbolTable) -> (Rc<RefCell<Module>>, Function) {
        // TODO: why are we passing `scope` to the compiler twice?
        let mut compiler = Compiler::new(&scope);
        match compiler.compile(&ast, &scope) {
            Ok(res) => res,
            Err(_) => todo!(),
        }
    }

    pub fn interpret(&mut self, script: Function, module: Rc<RefCell<Module>>) {
        let mut vm = VM::new(&self.config);

        vm.interpret(script, module);
    }

    pub fn run_from_source(&mut self, src: Rc<Source>) {
        let ast = self.parse_source(src.clone());
        let scope = self.check(&ast);
        let (module, script) = self.compile(&ast, &scope);
        self.interpret(script, module);
    }
}
