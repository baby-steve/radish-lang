//! Module containing Radish's runtime implementation and
//! its datastructures.

use crate::{
    common::{resolver::FileResolver, CompiledModule, Module},
    compiler::pipeline::CompilerPipeLine,
    config::Config,
};

use self::stack::Stack;

mod eval;
pub mod from_value;
mod load;
pub(crate) mod native;
mod run;
mod stack;
pub mod trace;
pub mod value;

use value::Closure;

#[derive(Debug)]
pub struct CallFrame {
    /// Call frame's function.
    pub closure: Closure,
    /// Track where we're at in the function's chunk.
    pub ip: usize,
    /// VM stack offset.
    pub offset: usize,
}

#[derive(Debug)]
pub struct VM {
    config: Box<Config>,
    stack: Stack,
    frames: Vec<CallFrame>,
    frame_count: usize,
    last_module: CompiledModule,
    modules: Vec<CompiledModule>,
    resolver: FileResolver,
    pipeline: CompilerPipeLine,
}

impl VM {
    pub fn new() -> Self {
        VM::with_config(Config::new())
    }

    pub fn with_config(config: Config) -> Self {
        let config = Box::new(config);

        let pipeline = CompilerPipeLine::new(&config).with_default_passes();

        Self {
            config,
            stack: Stack::new(),
            frames: Vec::new(),
            frame_count: 0,
            last_module: Module::empty(),
            modules: Vec::new(),
            resolver: FileResolver::new(),
            pipeline,
        }
    }
}

impl Default for VM {
    fn default() -> Self {
        VM::new()
    }
}
