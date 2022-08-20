use crate::common::{ModuleBuilder, RegisterFn};
use crate::{Namespace, NamespaceBuilder, RadishError, VM};

impl VM {
    pub fn load_fn<A, R>(
        &mut self,
        _name: impl ToString,
        _f: impl RegisterFn<A, R>,
    ) -> Result<(), RadishError> {
        todo!()
    }

    /// Load a [`Module`][crate::common::module::Module] into the VM.
    ///
    /// # Errors
    ///
    /// This function will return an error if it fails to build the module.
    ///
    /// # Examples
    ///
    /// Basic usage.
    /// ```
    /// # fn main() -> Result<(), radish::RadishError> {
    /// use radish::{VM, Module};
    ///
    /// let module = Module::new("example_module");
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
    ///         Ok(Module::new("some_module"))
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

        self.loader.load_module(&name, module);

        Ok(self)
    }

    // TODO: change the doc comment's example to adding the core library namespace.
    /// Load a [`Namespace`][crate::namespace::Namespace].
    ///
    /// # Examples
    ///
    /// ```no_run,
    /// # fn main() -> Result<(), radish::RadishError> {
    /// use radish::{VM, Namespace, Module, NamespaceBuilder};
    ///
    /// struct SomeNamespace;
    ///
    /// impl Namespace for SomeNamespace {
    ///     fn name(&self) -> &str {
    ///         "some_namespace"
    ///     }
    ///
    ///     fn build(&mut self, namespace: &mut NamespaceBuilder) {
    ///         namespace
    ///             .add(Module::new("mod"))
    ///             .add(Module::new("other"));
    ///     }
    /// }
    ///
    /// let mut vm = VM::new();
    /// vm.load_namespace(SomeNamespace);
    ///
    /// # Ok(())
    /// # }
    /// ```
    pub fn load_namespace<N: Namespace>(&mut self, mut namespace: N) -> &mut Self {
        let mut builder = NamespaceBuilder::new();

        namespace.build(&mut builder);

        builder.finish(self, namespace.name().to_string());

        self
    }

    pub fn load_const(&mut self) -> Result<(), RadishError> {
        todo!();
    }
}
