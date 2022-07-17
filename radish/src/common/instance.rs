use std::{rc::Rc, collections::HashMap, fmt, cell::RefCell};

use crate::Value;

use super::{Class, ImmutableString};

#[derive(Debug, PartialEq)]
pub struct Instance {
    class: Rc<Class>,
    pub fields: RefCell<HashMap<ImmutableString, Value>>,
}

impl Instance {
    pub fn new(class: &Rc<Class>) -> Self {
        let class = Rc::clone(class);
        let fields = RefCell::new(HashMap::new());

        Self {
            class,
            fields,
        }
    }

    pub fn class(&self) -> &Rc<Class> {
        &self.class
    }
}

impl PartialOrd for Instance {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        None
    }
}

impl fmt::Display for Instance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let class = self.class.name();
        write!(f, "<instance {class}>")
    }
}
