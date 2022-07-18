use crate::{Value, Trace, common::{ClassBuilder, Class}};

type VmResult = Result<Value, Trace>;

fn push(this: Value, value: Value) -> VmResult {
    this.into_array()?.borrow_mut().push(value);
    Ok(Value::Nil)
}

fn pop(this: Value) -> VmResult {
    Ok(this.into_array()?.borrow_mut().pop().unwrap_or(Value::Nil))
}

pub fn register() -> Class {
    ClassBuilder::new("Array")
        .add_method("push", push)
        .add_method("pop", pop)
        .build()
}
