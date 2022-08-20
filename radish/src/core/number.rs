use crate::{common::{Class, ClassBuilder}, Value, Trace, ToValue};

fn sqrt(this: Value) -> Result<Value, Trace> {
    Ok(this.into_number()?.sqrt().to_value())
}

pub fn register() -> Class {
    ClassBuilder::new("Number")
        .add_method("sqrt", sqrt)
        .build()
}
