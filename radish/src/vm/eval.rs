//! Module containing the VM's evaluation methods.

use crate::{
    common::{CompiledModule},
    vm::{trace::Trace, VM, from_value::FromValue, value::Closure},
    RadishError,
    Value,
};

impl VM {
    /// Execute a string.
    ///
    /// **NOTE**: if you need a return value, use [`eval()`][crate::vm::eval].
    ///
    /// # Examples
    ///  
    /// ```
    /// # fn main() -> Result<(), radish::RadishError> {
    /// use radish::VM;
    ///
    /// let mut vm = VM::new();
    ///
    /// assert_eq!(vm.exec("1 + 2"), Ok(()));
    ///
    /// #    Ok(())
    /// # }
    /// ```
    pub fn exec(&mut self, src: &str) -> Result<(), RadishError> {
        self._eval(src)?;
        Ok(())
    }

    /// Evaluate a string, returning its value.
    ///
    /// **NOTE**: if you do not require a return value, use [`exec()`][crate::vm::exec].
    ///
    /// # Examples
    ///
    /// ```no_run
    /// # fn main() -> Result<(), radish::RadishError> {
    /// use radish::VM;
    ///
    /// let mut vm = VM::new();
    ///
    /// assert_eq!(vm.eval::<i32>("1 + 2"), Ok(3));
    ///
    /// # Ok(())
    /// # }
    /// ```
    pub fn eval<I: FromValue>(&mut self, src: &str) -> Result<I, RadishError> {
        self._eval(src)
    }

    /// Execute a file.
    ///
    /// **NOTE**: if you require a return value, use [`eval_file()`][crate::vm::eval_file].
    ///
    /// # Examples
    ///
    /// ```no_run
    /// # fn main() -> Result<(), radish::RadishError> {
    /// use radish::VM;
    ///
    /// let mut vm = VM::new();
    ///
    /// assert_eq!(vm.exec_file("path/to/file.rdsh"), Ok(()));
    ///
    /// # Ok(())
    /// # }
    /// ```
    pub fn exec_file(&mut self, file_name: &str) -> Result<(), RadishError> {
        self._eval_file(file_name)?;
        Ok(())
    }

    /// Evaluate a file, returning its value.
    ///
    /// **NOTE**: if you do not require a return value, use [`exec_file()`][crate::vm::exec_file].
    ///
    /// # Examples
    ///
    /// ```no_run
    /// # fn main() -> Result<(), radish::RadishError> {
    /// use radish::VM;
    ///
    /// let mut vm = VM::new();
    ///
    /// assert_eq!(vm.eval::<()>("nil"), Ok(()));
    ///
    /// # Ok(())
    /// # }
    /// ```
    pub fn eval_file<I: FromValue>(&mut self, file_name: &str) -> Result<I, RadishError> {
        match self._eval_file(file_name) {
            Ok(val) => I::from_value(val),
            Err(e) => Err(e),
        }
    }

    /// Evaluate a file.
    fn _eval_file(&mut self, file_name: &str) -> Result<Value, RadishError> {
        let module = self.resolver.resolve(None, file_name, &mut self.pipeline)?;

        match self.interpret(module) {
            Ok(res) => Ok(res),
            Err(e) => Err(e.into()),
        }
    }

    /// Evaluate a string.
    fn _eval<I: FromValue>(&mut self, src: &str) -> Result<I, RadishError> {
        let module = self.pipeline.compile(&self.config.default_filename, src)?;

        match self.interpret(module) {
            Ok(val) => I::from_value(val),
            Err(e) => Err(e.into()),
        }
    }

    /// Interprete a compiled module.
    fn interpret(&mut self, module: CompiledModule) -> Result<Value, Trace> {
        self.last_module = module;

        let closure = Closure {
            function: self.last_module.borrow().entry(),
        };
        self.stack.push(Value::Closure(closure.clone()));
        self.call_function(closure, 0)?;

        let res = self.run()?;

        Ok(res)
    }
}
