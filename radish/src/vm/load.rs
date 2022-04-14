use crate::{common::module::ModuleBuilder, RadishError, VM};

impl VM {
    pub fn load_fn(&mut self) -> Result<(), RadishError> {
        todo!();
    }

    /// Load a [`Module`][crate::common::module::Module] into the VM.
    ///
    /// # Examples
    /// 
    /// Basic usage.
    /// ```
    /// # fn main() -> Result<(), radish::RadishError> {
    /// use radish::{VM, Module};
    ///
    /// let module = Module::new_("example_module");
    ///
    /// let mut vm = VM::new();
    ///
    /// vm.load_module(module);
    ///
    /// # Ok(())
    /// # }
    /// ```
    /// 
    /// Can also load a struct that impls [`ModuleBuilder`][crate::common::module::ModuleBuilder].
    /// ```
    /// # fn main() -> Result<(), radish::RadishError> {
    /// use radish::{VM, Module, ModuleBuilder};
    ///
    /// struct SomeModule;
    /// 
    /// impl ModuleBuilder for SomeModule {
    ///     fn build(self) -> Result<Module, String> {
    ///         Ok(Module::new_("some_module"))
    ///     }
    /// }
    /// 
    /// let mut vm = VM::new();
    ///
    /// vm.load_module(SomeModule);
    ///
    /// # Ok(())
    /// # }
    pub fn load_module(&mut self, module: impl ModuleBuilder) -> Result<&mut Self, RadishError> {
        let module = module.build()?;

        let name = module.name.to_string();

        self.resolver.load(name, module);

        Ok(self)
    }

    pub fn load_const(&mut self) -> Result<(), RadishError> {
        todo!();
    }

    pub fn load_native_fn(&mut self) -> Result<(), RadishError> {
        todo!();
    }
}
