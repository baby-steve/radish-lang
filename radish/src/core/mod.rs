use crate::{
    common::{Module, ModuleBuilder, Value},
    vm::trace::Trace,
    Namespace, VM, ToValue,
};

pub fn test(_vm: &mut VM) -> Result<Value, Trace> {
    println!("this is just for testing");

    Ok(Value::Nil)
}

fn clock() -> Result<Value, Trace> {
    use std::time::{SystemTime, UNIX_EPOCH};

    let start = SystemTime::now();
    let time = start
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards")
        .as_secs_f64();

    Ok(time.to_value())
}

pub struct System;

impl ModuleBuilder for System {
    fn build(self) -> Result<Module, String> {
        let mut module = Module::new("sys");

        module.add_native("test_fun", 0, test);
        module.add_native("clock", 0, clock);

        Ok(module)
    }
}

pub struct Math;

impl ModuleBuilder for Math {
    fn build(self) -> Result<Module, String> {
        let mut module = Module::new("math");

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
