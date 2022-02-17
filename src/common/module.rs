use crate::common::{value::Function, Value};

use std::{cell::RefCell, cmp::Ordering, collections::HashMap, fmt, rc::Rc};

pub type CompiledModule = Rc<RefCell<Module>>;

#[derive(Debug, Default)]
pub struct Module {
    pub name: Box<str>,
    pub variables: Vec<Value>,
    pub symbols: HashMap<String, usize>,
}

impl Module {
    pub fn new(name: &str) -> Rc<RefCell<Module>> {
        let module = Module {
            name: name.to_string().into_boxed_str(),
            variables: Vec::new(),
            symbols: HashMap::new(),
        };
        Rc::new(RefCell::new(module))
    }

    #[inline]
    pub fn add_var(&mut self, name: String) -> usize {
        let index = self.variables.len();
        self.variables.push(Value::Nil);

        self.symbols.insert(name, index);

        index
    }

    #[inline]
    pub fn get_index(&self, name: &str) -> Option<usize> {
        if let Some(index) = self.symbols.get(name) {
            Some(*index)
        } else {
            None
        }
    }

    #[inline]
    pub fn set_var(&mut self, index: usize, value: Value) {
        self.variables[index] = value;
    }

    #[inline]
    pub fn get_var(&self, index: usize) -> &Value {
        &self.variables[index]
    }

    pub(crate) fn add_entry(&mut self, fun: Function) {
        let index = self.variables.len();

        self.variables.push(Value::from(fun));

        self.symbols.insert(String::from(""), index);
    }

    pub fn entry(&self) -> Rc<Function> {
        if let Some(index) = self.symbols.get("") {
            match &self.variables[*index] {
                Value::Function(fun) => Rc::clone(&fun),
                _ => unreachable!(
                    "The entry point for module '{}' is not a function.",
                    self.name.to_string()
                ),
            }
        } else {
            panic!(
                "Module '{}' doesn't have an entry point",
                self.name.to_string()
            );
        }
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
                if name == "" { "entry" } else { &name },
                value
            )?;
        }

        write!(f, "")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::{value::Function, Chunk};

    #[test]
    fn variables() {
        let module = Module::new("test");
        let mut module = module.borrow_mut();

        module.add_var("a".to_string());

        let value = module.get_var(0);
        assert_eq!(value, &Value::Nil);

        module.set_var(0, Value::Boolean(true));

        let value = module.get_var(0);
        assert_eq!(value, &Value::Boolean(true));
    }

    #[test]
    fn entry() {
        let module = Module::new("test");

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

        assert_eq!(module.borrow().entry(), Rc::new(fun));
    }
}
