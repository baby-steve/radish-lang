use std::fmt;

use crate::{
    common::{Source, CompiledModule},
    compiler::Compiler, compiler::Parser, compiler::SyntaxError, compiler::AST, config::Config,
};

use super::{validate_ast, resolve_symbols, hoist::hoist};

type ASTPass = Box<dyn FnMut(&mut AST) -> Result<(), SyntaxError> + 'static>;

pub struct PipelineSettings {
    pub dump_bytecode: bool,
    pub dump_ast: bool,
}

impl PipelineSettings {
    pub fn new() -> Self {
        Self {
            dump_bytecode: false,
            dump_ast: false,
        }
    }
}

impl Default for PipelineSettings {
    fn default() -> Self {
        PipelineSettings::new()
    }
}

impl From<&Config> for PipelineSettings {
    fn from(config: &Config) -> Self {
        Self {
            dump_bytecode: config.dump_bytecode,
            dump_ast: config.dump_ast,
        }
    }
}

pub struct CompilerPipeLine {
    settings: PipelineSettings,
    compiler: Compiler,
    passes: Vec<ASTPass>,
}

impl CompilerPipeLine {
    pub fn new(config: &Config) -> Self {
        let settings = PipelineSettings::from(config);
        let compiler = Compiler::new(&settings);

        Self {
            settings,
            passes: vec![],
            compiler,
        }
    }

    pub fn with_default_passes(mut self) -> Self {
        self.register_pass(resolve_symbols);
        self.register_pass(validate_ast);
        self.register_pass(hoist);
        
        self
    }

    pub fn register_pass<F: 'static>(&mut self, pass: F) -> &mut Self
    where
        F: FnMut(&mut AST) -> Result<(), SyntaxError>,
    {
        self.passes.push(Box::new(pass));
        self
    }

    pub fn compile(&mut self, file_name: &str, src: &str) -> Result<CompiledModule, SyntaxError> {
        self._compile(file_name, src)
    }

    fn _compile(&mut self, file_name: &str, src: &str) -> Result<CompiledModule, SyntaxError> {
        let source = Source::new(src, &file_name);

        let mut parser = Parser::new(source, &self.settings);

        let mut ast = parser.parse()?;

        for callback in self.passes.iter_mut() {
            ast.visit(callback)?;
        }

        let module = self.compiler.compile(file_name, &ast)?;

        Ok(module)
    }
}

impl fmt::Debug for CompilerPipeLine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "CompilerPipeLine {{ .. }}")
    }
}
