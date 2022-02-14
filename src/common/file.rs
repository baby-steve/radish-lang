use std::fmt::{Debug};

pub trait RadishFile {
    fn write(&self, msg: &str);
}

impl Debug for dyn RadishFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result {
        writeln!(f, "__RadishFile__")
    }
}