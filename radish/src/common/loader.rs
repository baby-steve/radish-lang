use std::{cell::RefCell, collections::BTreeMap, fs, rc::Rc};

use crate::{
    common::{resolver::Resolver, CompiledModule},
    Module, RadishError, compiler::pipeline::CompilerPipeLine,
};

#[derive(Debug)]
pub struct Loader {
    resolver: Resolver,
    cache: BTreeMap<String, CompiledModule>,
}

impl Default for Loader {
    fn default() -> Self {
        todo!()
    }
}

impl Loader {
    pub fn new() -> Self {
        Self {
            resolver: Resolver::new(),
            cache: BTreeMap::new(),
        }
    }

    pub fn is_cached(&self, path: &str) -> bool {
        let file_path = path;

        if !self.cache.is_empty() {
            self.cache.contains_key(file_path)
        } else {
            false
        }
    }

    pub fn load(
        &mut self,
        path: &str,
        compiler: &mut CompilerPipeLine,
    ) -> Result<CompiledModule, RadishError> {
        let name = self.resolver.resolve(path)?;

        // TODO: handle chached files here? 
        if self.is_cached(&name) {
            let module = self.cache.get(&name);
            return Ok(Rc::clone(module.unwrap()))
        } else if let Ok(src) = fs::read_to_string(&name) {
            return self.load_file(&src, &name, compiler)
        } else {
            panic!("failed to import module {}", name);
        }
    }

    pub fn load_file(
        &mut self,
        src: &str,
        path: &str,
        compiler: &mut CompilerPipeLine,
    ) -> Result<CompiledModule, RadishError> {
        let name = path;

        if let Some(module) = self.cache.get(name) {
            return Ok(module.clone());
        }

        let module = compiler.compile(name, &src)?;

        self.cache.insert(name.to_string(), module.clone());

        Ok(module)
    }

    pub fn load_module(&mut self, name: &str, module: Module) {
        let mod_ref = Rc::new(RefCell::new(module));
        self.cache.insert(name.to_string(), mod_ref);
    }

    pub fn load_ref(&mut self, name: &str, module: CompiledModule) {
        self.cache.insert(name.to_string(), module);
    }
}
