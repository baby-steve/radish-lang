use std::{cell::RefCell, rc::Rc};

use crate::Value;

pub trait ToValue {
    fn to_value(self) -> Value;
}

impl ToValue for () {
    fn to_value(self) -> Value {
        Value::Nil
    }
}

impl ToValue for bool {
    fn to_value(self) -> Value {
        Value::Boolean(self)
    }
}

impl ToValue for Value {
    fn to_value(self) -> Value {
        self
    }
}

macro_rules! impl_number {
    ($num: ty) => {
        impl ToValue for $num {
            fn to_value(self) -> Value {
                Value::Number(self as f64)
            }
        }
    };
}

impl_number!(f64);
impl_number!(f32);
impl_number!(i8);
impl_number!(i16);
impl_number!(i32);
impl_number!(i64);
impl_number!(i128);
impl_number!(isize);
impl_number!(u8);
impl_number!(u16);
impl_number!(u32);
impl_number!(u64);
impl_number!(u128);
impl_number!(usize);

impl ToValue for &str {
    fn to_value(self) -> Value {
        Value::String(Rc::new(RefCell::new(self.to_string())))
    }
}

impl ToValue for &mut str {
    fn to_value(self) -> Value {
        Value::String(Rc::new(RefCell::new(self.to_string())))
    }
}

impl ToValue for Box<str> {
    fn to_value(self) -> Value {
        Value::String(Rc::new(RefCell::new(self.to_string())))
    }
}

impl ToValue for &String {
    fn to_value(self) -> Value {
        Value::String(Rc::new(RefCell::new(self.clone())))
    }
}

impl ToValue for String {
    fn to_value(self) -> Value {
        Value::String(Rc::new(RefCell::new(self)))
    }
}
