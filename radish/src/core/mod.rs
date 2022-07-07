use crate::{
    common::{module::ModuleBuilder, Module},
    Value, VM, Namespace, vm::trace::Trace,
};

pub fn test(_vm: &mut VM, _args: Vec<Value>) -> Result<Value, Trace> {
    println!("this is just for testing");

    Ok(Value::Nil)
}

pub struct System;

impl ModuleBuilder for System {
    fn build(self) -> Result<Module, String> {
        let mut module = Module::new_("sys");

        module.add_native("test_fun", 0, test);

        Ok(module)
    }
}

pub struct Math;

impl ModuleBuilder for Math {
    fn build(self) -> Result<Module, String> {
        let mut module = Module::new_("math");

        module.add_value("pi", 3.14);

        Ok(module)
    }
}

pub struct RadishCore;

impl Namespace for RadishCore {
    fn name(&self) -> &str {
        "std"
    }

    fn build(&mut self, namespace: &mut crate::NamespaceBuilder) {
        namespace.add(System).add(Math);    
    }
}
