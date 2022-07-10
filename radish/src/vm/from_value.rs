use std::{cell::RefCell, rc::Rc};

use crate::{Module, Value};
use super::trace::Trace;

pub trait FromValue: Sized {
    fn from_value(val: Value) -> Result<Self, Trace>;
}

impl FromValue for () {
    fn from_value(val: Value) -> Result<Self, Trace> {
        match val {
            Value::Nil => Ok(()),
            _ => Err(Trace::new("cannot coerce type into unit")),
        }
    }
}

impl FromValue for bool {
    fn from_value(val: Value) -> Result<Self, Trace> {
        match val {
            Value::Boolean(true) => Ok(true),
            Value::Boolean(false) => Ok(false),
            _ => Err(Trace::new("cannot coerce type into boolean")),
        }
    }
}

macro_rules! impl_number {
    ($typ:ty) => {
        impl FromValue for $typ {
            fn from_value(val: Value) -> Result<Self, Trace> {
                let num = match val {
                    Value::Number(val) => val,
                    _ => return Err(Trace::new("cannot coerce type into number")),
                };

                Ok(num as $typ)
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

impl FromValue for Rc<RefCell<Module>> {
    fn from_value(val: Value) -> Result<Self, Trace> {
        val.into_module().map_err(|err| Trace::new(err))
    }
}

impl FromValue for Value {
    fn from_value(val: Value) -> Result<Self, Trace> {
        Ok(val)
    }
}
