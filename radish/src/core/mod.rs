use crate::{
    common::{Module, ModuleBuilder, Value, Class},
    vm::trace::Trace,
    Namespace, ToValue,
};

mod array;
mod string;
mod number;
mod map;

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

#[derive(Debug)]
pub struct BuiltinClasses {
    pub bool_class: Class,
    pub number_class: Class,
    pub string_class: Class,
    pub array_class: Class,
    pub map_class: Class,
    pub function_class: Class,
}

impl Default for BuiltinClasses {
    fn default() -> Self {
        Self {
            bool_class: Class::new("Bool"),
            number_class: number::register(),
            string_class: string::register(),
            array_class: array::register(),
            map_class: map::register(),
            function_class: Class::new("Function"),
        }
    }
}
