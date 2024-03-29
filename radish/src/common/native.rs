use std::{
    cmp::Ordering,
    fmt::{self},
    rc::Rc, cell::RefCell,
};

use crate::{vm::trace::Trace, VM};

use super::Value;

pub type InnerFn = dyn Fn(&mut VM, Vec<Value>) -> Result<Value, Trace>;

pub struct NativeFunction {
    pub fun: Rc<InnerFn>,
    pub arity: u8,
}

impl NativeFunction {
    pub fn new<F: 'static>(fun: Rc<InnerFn>, arity: u8) -> Self {
        Self { fun, arity }
    }
}

impl PartialOrd for NativeFunction {
    fn partial_cmp(&self, _other: &Self) -> Option<Ordering> {
        None
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        // FIXME: clippy doesn't like that we compare two trait object pointers.
        Rc::ptr_eq(&self.fun, &other.fun) && self.arity == other.arity
    }
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("NativeFunction")
            .field("inner", &"Rc<InnerFn>")
            .field("airty", &self.arity)
            .finish()
    }
}

impl Clone for NativeFunction {
    fn clone(&self) -> Self {
        Self {
            fun: Rc::clone(&self.fun),
            arity: self.arity,
        }
    }
}

pub type InnerMethod = dyn Fn(&mut VM, Value, Vec<Value>) -> Result<Value, Trace>;

#[derive(Clone)]
pub struct NativeMethod {
    pub reciever: Value,
    pub inner: Rc<InnerMethod>,
}

impl PartialOrd for NativeMethod {
    fn partial_cmp(&self, _: &Self) -> Option<Ordering> {
        None
    }
}

impl PartialEq for NativeMethod {
    fn eq(&self, other: &Self) -> bool {
        // FIXME: clippy doesn't like that we compare two trait object pointers.
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}

impl fmt::Debug for NativeMethod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NativeMethod {{ inner: Rc<Fn> }}")
    }
}

impl NativeMethod {
    pub fn new(method: Rc<InnerMethod>, this: Value) -> Self {
        Self {
            inner: method,
            reciever: this,
        }
    }

    pub fn call(&self, vm: &mut VM, args: Vec<Value>) -> Result<Value, Trace> {
        (self.inner)(vm, self.reciever.clone(), args)
    }
}
