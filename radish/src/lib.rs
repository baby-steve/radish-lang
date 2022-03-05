pub mod cli;
pub mod common;
pub mod compiler;
pub mod error;
pub mod vm;

pub use cli::Cli;

use std::fmt;
use std::rc::Rc;

use common::{source::Source, CompiledModule};
use compiler::{
    analysis::Analyzer, ast::AST, compiler::Compiler, error::SyntaxError, parser::Parser,
    table::SymbolTable,
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
        IOError(std::sync::Arc::new(err.into()))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RadishError {
    CompilerError(SyntaxError),
    RuntimeError(Trace),
    IOError(IOError),
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
        }
    }
}

#[derive(Debug)]
pub struct RadishConfig {
    stdout: Rc<dyn RadishFile>,
    dump_ast: bool,
    dump_code: bool,
    trace: bool,
}

impl Default for RadishConfig {
    fn default() -> RadishConfig {
        RadishConfig {
            stdout: Rc::new(RadishIO),
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

    pub fn from_cli(cli: &Cli) -> Rc<RadishConfig> {
        Rc::new(RadishConfig {
            dump_ast: cli.dump_ast,
            dump_code: cli.dump_code,
            trace: cli.trace,
            ..RadishConfig::default()
        })
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
        Radish { config }
    }

    pub fn parse_source(&mut self, source: Rc<Source>) -> Result<AST, RadishError> {
        let mut parser = Parser::new(source, &self.config);
        match parser.parse() {
            Ok(ast) => Ok(ast),
            Err(err) => {
                // TODO: replace this with config's own stderr.
                //use termcolor::{ColorChoice, StandardStream};
                //let mut temp_stderr = StandardStream::stderr(ColorChoice::Always);
                //error::emit(&mut temp_stderr, &err.report(), 1).unwrap();

                //panic!("error: failed to parse");
                Err(RadishError::from(err))
            }
        }
    }

    pub fn check(&mut self, ast: &AST) -> Result<SymbolTable, RadishError> {
        let mut analyzer = Analyzer::new(&self.config);
        match analyzer.analyze(ast) {
            Ok(table) => Ok(table),
            Err(err) => Err(err.into()),
        }
    }

    pub fn compile(&mut self, ast: &AST, scope: &SymbolTable) -> CompiledModule {
        // TODO: why are we passing `scope` to the compiler twice?
        let mut compiler = Compiler::new(&scope, &self.config);
        match compiler.compile(&ast, &scope) {
            Ok(res) => res,
            Err(_) => todo!(),
        }
    }

    pub fn interpret(&mut self, module: CompiledModule) -> Result<(), RadishError> {
        let mut vm = VM::new(&self.config);

        let res = vm.interpret(module)?;

        Ok(res)
    }

    pub fn run_from_source(&mut self, src: Rc<Source>) -> Result<(), RadishError> {
        let ast = self.parse_source(src)?;
        let scope = self.check(&ast)?;

        let module = self.compile(&ast, &scope);

        let res = self.interpret(module)?;

        Ok(res)
    }

    pub fn read_file(&self, path: &str) -> Result<Rc<Source>, RadishError> {
        let path_buf = std::path::PathBuf::from(path);
        let result = std::fs::read_to_string(&path_buf);

        let content = match result {
            Ok(res) => res,
            Err(error) => return Err(error.into()),
        };

        Ok(Source::new(&content, path_buf.to_string_lossy()))
    }
}
