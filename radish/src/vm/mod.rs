//! Module containing Radish's runtime implementation and
//! its datastructures.

use std::{collections::HashMap, rc::Rc};

use crate::{
    common::{loader::Loader, CompiledModule, Module},
    compiler::pipeline::CompilerPipeLine,
    config::Config,
    RadishCore,
};

use self::stack::Stack;

mod eval;
pub mod from_value;
mod load;
pub(crate) mod native;
mod run;
mod stack;
pub mod to_value;
pub mod trace;
pub mod value;

use value::Closure;

#[derive(Debug)]
pub struct CallFrame {
    /// Call frame's function.
    pub closure: Rc<Closure>,
    /// Track where we're at in the function's chunk.
    pub ip: usize,
    /// VM stack offset.
    pub offset: usize,
}

#[derive(Debug)]
pub struct VM {
    /// VM configuration.
    config: Box<Config>,
    /// VM's operator stack.
    stack: Stack,
    /// VM's call stack.
    frames: Vec<CallFrame>,
    /// Number of frame's current on the call stack.
    frame_count: usize,

    /// Store upvalues for later access by closures.
    /// Contains the locations of non-local values on the stack.
    upvalues: HashMap<usize, usize>,

    /// A reference to the last module load by the VM.
    last_module: CompiledModule,
    /// A list of all modules loaded into the VM.
    modules: Vec<CompiledModule>,
    /// Frontend pipeline.
    compiler: CompilerPipeLine,
    /// VM's loader.
    pub(crate) loader: Loader,
}

impl VM {
    pub fn new() -> Self {
        let mut vm = VM::with_config(Config::new());

        vm.load_namespace(RadishCore)
            .expect("Failed to load standard library");

        vm
    }

    pub fn with_config(config: Config) -> Self {
        let config = Box::new(config);

        let pipeline = CompilerPipeLine::new(&config).with_default_passes();

        Self {
            config,
            stack: Stack::new(),
            frames: Vec::new(),
            frame_count: 0,
            upvalues: HashMap::new(),
            last_module: Module::empty(),
            modules: Vec::new(),
            loader: Loader::new(),
            compiler: pipeline,
        }
    }
}

impl Default for VM {
    fn default() -> Self {
        VM::new()
    }
}
