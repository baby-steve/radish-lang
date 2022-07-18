use crate::{common::{Class, ClassBuilder}, Value, Trace, ToValue};

fn contains(this: Value, key: Value) -> Result<Value, Trace> {
    Ok(this.into_map()?.borrow().contains_key(&key.to_string()).to_value())
}

pub fn register() -> Class {
    ClassBuilder::new("Map")
        .add_method("contains", contains)
        .build()
}
