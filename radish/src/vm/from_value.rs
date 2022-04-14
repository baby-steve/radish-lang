use crate::{Value, RadishError};

pub trait FromValue: Sized {
    fn from_value(val: Value) -> Result<Self, RadishError>;
}

impl FromValue for () {
    fn from_value(val: Value) -> Result<Self, RadishError> {
        match val {
            Value::Nil => Ok(()),
            _ => Err("cannot coerce type into unit".into()),
        }
    }
}

impl FromValue for bool {
    fn from_value(val: Value) -> Result<Self, RadishError> {
        match val {
            Value::Boolean(true) => Ok(true),
            Value::Boolean(false) => Ok(false),
            _ => Err("cannot coerce type into boolean".into()),
        }
    }
}

macro_rules! impl_number {
    ($typ:ty) => {
        impl FromValue for $typ {
            fn from_value(val: Value) -> Result<Self, RadishError> {
                let num = match val {
                    Value::Number(val) => val,
                    _ => return Err("cannot coerce type into number".into()),
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
