//! Module containing Radish's runtime implementation and
//! its datastructures.

use std::{collections::HashMap, rc::Rc};

use crate::{
    common::{Closure, CompiledModule, Loader, Module},
    compiler::pipeline::CompilerPipeLine,
    config::Config,
    RadishCore, core::BuiltinClasses,
};

mod eval;
mod load;
mod run;
mod stack;
// mod error;
pub(crate) use stack::Stack;

pub mod trace;

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
    pub(crate) stack: Stack,
    /// VM's call stack.
    pub(crate) frames: Vec<CallFrame>,
    /// Number of frame's currently on the call stack.
    pub(crate) frame_count: usize,
    /// Vm's builtin classes.
    builtins: BuiltinClasses,

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
        VM::with_config(Config::new())
    }

    pub fn with_config(config: Config) -> Self {
        let config = Box::new(config);

        let pipeline = CompilerPipeLine::new(&config).with_default_passes();

        let mut vm = Self {
            config,
            stack: Stack::new(),
            frames: Vec::new(),
            frame_count: 0,
            builtins: BuiltinClasses::default(),
            upvalues: HashMap::new(),
            last_module: Module::empty(),
            modules: Vec::new(),
            loader: Loader::new(),
            compiler: pipeline,
        };

        vm.load_namespace(RadishCore);

        vm
    }
}

impl Default for VM {
    fn default() -> Self {
        VM::new()
    }
}
