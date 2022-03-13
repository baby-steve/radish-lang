pub mod common;
pub mod compiler;
pub mod error;
pub mod vm;

use std::fmt;
use std::rc::Rc;

use common::{source::Source, CompiledModule, Value};
use compiler::{
    ast::AST, compiler::Compiler, error::SyntaxError, parser::Parser,
    scope::ScopeMap,
};

use vm::{trace::Trace, vm::VM};

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

#[derive(Debug, Clone)]
pub struct IOError(std::sync::Arc<std::io::Error>);

impl fmt::Display for IOError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Eq for IOError {}

impl PartialEq for IOError {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(&*self.0, &*other.0)
    }
}

impl From<std::io::Error> for IOError {
    fn from(err: std::io::Error) -> IOError {
        IOError(std::sync::Arc::new(err))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RadishError {
    CompilerError(SyntaxError),
    RuntimeError(Trace),
    IOError(IOError),
    Other(String),
}

impl From<SyntaxError> for RadishError {
    fn from(err: SyntaxError) -> RadishError {
        RadishError::CompilerError(err)
    }
}

impl From<Trace> for RadishError {
    fn from(err: Trace) -> RadishError {
        RadishError::RuntimeError(err)
    }
}

impl From<std::io::Error> for RadishError {
    fn from(err: std::io::Error) -> RadishError {
        RadishError::IOError(err.into())
    }
}

impl From<String> for RadishError {
    fn from(err: String) -> Self {
        RadishError::Other(err)
    }
}

impl From<&str> for RadishError {
    fn from(err: &str) -> Self {
        RadishError::Other(err.to_string())
    }
}

impl RadishError {
    pub fn emit(&self) {
        match &self {
            RadishError::CompilerError(err) => {
                use termcolor::{ColorChoice, StandardStream};
                let mut temp_stderr = StandardStream::stderr(ColorChoice::Always);
                error::emit(
                    &mut temp_stderr,
                    &err.report(),
                    error::DisplayStyle::Verbose,
                )
                .unwrap();
            }
            RadishError::RuntimeError(err) => print!("{}", err),
            RadishError::IOError(err) => print!("{}", err),
            RadishError::Other(err) => print!("{}", err),
        }
    }
}

// TODO: move debug info to its own struct.
#[derive(Debug)]
pub struct RadishConfig {
    pub stdout: Rc<dyn RadishFile>,
    pub repl: bool,
    pub dump_ast: bool,
    pub dump_code: bool,
    pub trace: bool,
}

impl Default for RadishConfig {
    fn default() -> RadishConfig {
        RadishConfig {
            stdout: Rc::new(RadishIO),
            repl: false,
            dump_ast: false,
            dump_code: false,
            trace: false,
        }
    }
}

impl RadishConfig {
    pub fn new() -> Rc<RadishConfig> {
        Rc::new(RadishConfig::default())
    }

    pub fn with_stdout(stdout: Rc<dyn RadishFile>) -> Rc<RadishConfig> {
        Rc::new(RadishConfig {
            stdout,
            ..RadishConfig::default()
        })
    }

    pub fn set_repl(&mut self, repl: bool) {
        self.repl = repl;
    }
}

type RadishResult<T> = Result<T, RadishError>;

/// The rudimentary wrapper of sorts for the Radish language.
pub struct Radish {
    config: Rc<RadishConfig>,
}

impl Radish {
    pub fn new() -> Radish {
        Radish::with_settings(RadishConfig::new())
    }

    pub fn with_settings(config: Rc<RadishConfig>) -> Radish {
        Radish { config }
    }

    // pub fn compile(&mut self, ast: &AST, scope: &SymbolTable) -> CompiledModule {
    //     // TODO: why are we passing `scope` to the compiler twice?
    //     let mut compiler = Compiler::new(&scope, &self.config);
    //     match compiler.compile(&ast, &scope) {
    //         Ok(res) => res,
    //         Err(_) => todo!(),
    //     }
    // }

    //pub fn interpret(&mut self, module: CompiledModule) -> Result<Value, RadishError> {
    //    let mut vm = VM::new(&self.config);

    //    let res = vm.interpret(module)?;

    //    Ok(res)
    //}

    //pub fn run_from_source(&mut self, src: Rc<Source>) -> Result<Value, RadishError> {
    //    let ast = self.parse_source(src)?;
    //    let scope = self.check(&ast)?;

    //    let module = self.compile(&ast, &scope);

    //    let res = self.interpret(module)?;

    //    Ok(res)
    //}

    //pub fn read_file(&self, path: &str) -> Result<Rc<Source>, RadishError> {
    //    let path_buf = std::path::PathBuf::from(path);
    //    let result = std::fs::read_to_string(&path_buf);

    //    let content = match result {
    //        Ok(res) => res,
    //        Err(error) => return Err(error.into()),
    //    };

    //    Ok(Source::new(&content, path_buf.to_string_lossy()))
    //}

    // parse a string
    pub fn parse_str(&mut self, filename: &str, source: &str) -> RadishResult<AST> {
        let src = Source::new(source, filename);
        let mut parser = Parser::new(src, &self.config);

        Ok(parser.parse()?)
    }


    pub fn check(&mut self, ast: &mut AST) -> RadishResult<()> {
        Ok(compiler::check(ast)?)
    }

    pub fn compile_script(&mut self, filename: &str, input: &str) -> RadishResult<CompiledModule> {
        let mut ast = self.parse_str(filename, input)?;

        Ok(self.compile_ast(&mut ast)?)
    }

    pub fn compile_file(&mut self, filename: &str) -> RadishResult<CompiledModule> {
        let path_buf = std::path::PathBuf::from(filename);
        let result = std::fs::read_to_string(&path_buf);

        Ok(self.compile_script(filename, &result?)?)
    }

    pub fn compile_ast(&mut self, ast: &mut AST) -> RadishResult<CompiledModule> {
        self.check(ast)?;
        let mut compiler = Compiler::new(&self.config);

        Ok(compiler.compile(&ast)?)
    }

    // load a string (and compile it?)
    fn _load_script(&mut self, _filename: &str, _input: &str) -> RadishResult<()> {
        todo!();
    }

    // load a string by reading a file with the given name (and compile it?)
    fn _load_file(&mut self, filename: &str) -> RadishResult<()> {
        let path_buf = std::path::PathBuf::from(filename);
        let result = std::fs::read_to_string(&path_buf);

        let content = match result {
            Ok(res) => res,
            Err(error) => return Err(error.into()),
        };

        self._load_script(filename, &content)
    }

    // run an expr, returning the result
    pub fn run_expr(&mut self, name: &str, expr_str: &str) -> RadishResult<Value> {
        let module = self.compile_script(name, expr_str)?;

        Ok(self.run_module(module)?)
    }

    pub fn run_module(&mut self, module: CompiledModule) -> RadishResult<Value> {
        let mut vm = VM::new(&self.config);

        Ok(vm.interpret(module)?)
    }
}
