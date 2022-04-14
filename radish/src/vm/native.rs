use std::{rc::Rc, cmp::Ordering, fmt};

use crate::{Value, VM};

pub type InnerFn = dyn FnMut(&mut VM, Vec<Value>) -> Result<Value, String>;

pub struct NativeFunction {
    pub fun: Rc<InnerFn>,
    pub airty: u8,
}

impl NativeFunction {
    pub fn new<F: 'static>(fun: Rc<InnerFn>, airty: u8) -> Self {
        Self { fun, airty }
    }
}

impl PartialOrd for NativeFunction {
    fn partial_cmp(&self, _other: &Self) -> Option<Ordering> {
        None
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.fun, &other.fun) && self.airty == other.airty
    }
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("NativeFunction")
            .field("inner", &"Rc<InnerFn>")
            .field("airty", &self.airty)
            .finish()
        }
}

impl Clone for NativeFunction {
    fn clone(&self) -> Self {
        Self {
            fun: Rc::clone(&self.fun),
            airty: self.airty,
        }
    }
}