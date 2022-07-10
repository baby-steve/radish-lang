use crate::vm::from_value::FromValue;
use crate::vm::native::NativeFunction;
use crate::vm::register::RegisterFn;
use crate::vm::to_value::ToValue;
use crate::vm::value::Function;
use crate::Value;

use std::{cell::RefCell, cmp::Ordering, collections::HashMap, fmt, rc::Rc};

pub type CompiledModule = Rc<RefCell<Module>>;

#[derive(Debug, Default)]
pub struct Module {
    pub name: Box<str>,
    pub variables: Vec<Value>,
    pub symbols: HashMap<String, usize>,
}

impl Module {
    pub fn new_ref(name: &str) -> Rc<RefCell<Module>> {
        let module = Self::new(name);
        Rc::new(RefCell::new(module))
    }

    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string().into_boxed_str(),
            variables: Vec::new(),
            symbols: HashMap::new(),
        }
    }

    pub(crate) fn empty() -> Rc<RefCell<Module>> {
        Module::new_ref("")
    }

    pub fn add_native<F: 'static, A, R>(&mut self, name: &str, airty: u8, fun: F) -> &mut Self
    where
        F: RegisterFn<A, R>
    {
        let native_fun = NativeFunction::new::<F>(fun.register(), airty);

        let index = self.add_symbol(name.to_string());

        self.set_value_at_index(index, Value::NativeFunction(Rc::new(native_fun)));

        self
    }

    pub fn add_module<M>(&mut self, module: M) -> &mut Self
    where
        M: ModuleBuilder,
    {
        let m = module.build().expect("failed to build module");

        let index = self.add_symbol(m.name.to_string());

        self.set_value_at_index(index, Value::Module(Rc::new(RefCell::new(m))));

        self
    }

    pub fn add_value<T>(&mut self, name: impl ToString, value: T) -> &mut Self
    where
        T: ToValue,
    {
        let index = self.add_symbol(name.to_string());

        self.set_value_at_index(index, value.to_value());

        self
    }

    pub fn get_variable<F: FromValue>(&self, name: &str) -> Option<F> {
        let index = match self.get_index(name) {
            Some(i) => i,
            None => return None,
        };

        let value = self.get_value_at_index(index).clone();

        Some(F::from_value(value).expect("Failed to coerce type"))
    }

    #[inline]
    pub(crate) fn add_symbol(&mut self, name: String) -> usize {
        let index = self.variables.len();
        self.variables.push(Value::Nil);

        self.symbols.insert(name, index);

        index
    }

    #[inline]
    pub(crate) fn get_index(&self, name: &str) -> Option<usize> {
        self.symbols.get(name).copied()
    }

    #[inline]
    pub(crate) fn set_value_at_index(&mut self, index: usize, value: Value) {
        self.variables[index] = value;
    }

    #[inline]
    pub(crate) fn get_value_at_index(&self, index: usize) -> &Value {
        &self.variables[index]
    }

    pub(crate) fn add_entry(&mut self, fun: Function) {
        let index = self.variables.len();

        self.variables.push(Value::from(fun));

        self.symbols.insert(String::from(""), index);
    }

    pub(crate) fn entry(&self) -> Option<Rc<Function>> {
        if let Some(index) = self.symbols.get("") {
            match &self.variables[*index] {
                Value::Function(fun) => Some(Rc::clone(fun)),
                _ => unreachable!(
                    "The entry point for module '{}' is not a function.",
                    self.name.to_string()
                ),
            }
        } else {
            //panic!("Module '{}' doesn't have an entry point", self.name);
            None
        }
    }

    pub(crate) fn has_entry(&self) -> bool {
        self.symbols.contains_key("")
    }

    pub(crate) fn modules(&self) -> Vec<CompiledModule> {
        self.variables
            .iter()
            .filter_map(|v| match v {
                Value::Module(m) => Some(Rc::clone(m)),
                _ => None,
            })
            .collect()
    }
}

impl PartialEq for Module {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl PartialOrd for Module {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Module {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(&other.name)
    }
}

impl Eq for Module {}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;

        for (name, index) in &self.symbols {
            let value = &self.variables[*index];
            write!(
                f,
                "\n    {} {}: {}",
                index,
                if name.is_empty() { "entry" } else { name },
                value
            )?;
        }

        write!(f, "")
    }
}

pub trait ModuleBuilder {
    fn build(self) -> Result<Module, String>;
}

impl ModuleBuilder for Module {
    fn build(self) -> Result<Module, String> {
        Ok(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::Chunk;

    #[test]
    fn variables() {
        let module = Module::new_ref("test");
        let mut module = module.borrow_mut();

        module.add_symbol("a".to_string());

        let value = module.get_value_at_index(0);
        assert_eq!(value, &Value::Nil);

        module.set_value_at_index(0, Value::Boolean(true));

        let value = module.get_value_at_index(0);
        assert_eq!(value, &Value::Boolean(true));
    }

    #[test]
    fn entry() {
        let module = Module::new_ref("test");

        let fun = Function {
            arity: 0,
            chunk: Chunk::default(),
            name: "".to_string().into_boxed_str(),
            module: std::rc::Rc::downgrade(&module),
        };

        {
            module.borrow_mut().add_entry(fun.clone());
        }

        assert_eq!(module.borrow().variables.len(), 1);

        assert_eq!(module.borrow().entry(), Some(Rc::new(fun)));
    }
}
