use std::{fmt, rc::Rc};

use super::{Closure, Instance};

#[derive(Debug, PartialEq, PartialOrd)]
pub struct BoundMethod {
    pub reciever: Rc<Instance>,
    pub method: Rc<Closure>,
}

impl BoundMethod {
    pub fn new(reciever: &Rc<Instance>, method: &Rc<Closure>) -> Self {
        let reciever = Rc::clone(reciever);
        let method = Rc::clone(method);

        Self { reciever, method }
    }
}

impl fmt::Display for BoundMethod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let class = self.reciever.class().name();
        let this = &*self.method.function.name;
        write!(f, "<bound method {this} of {class}>")
    }
}
