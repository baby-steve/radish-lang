use crate::{
    common::{Class, ClassBuilder},
    ToValue, Trace, Value,
};

fn len(this: Value) -> Result<Value, Trace> {
    Ok(this.into_string()?.len().to_value())
}

fn is_empty(this: Value) -> Result<Value, Trace> {
    Ok(this.into_string()?.is_empty().to_value())
}

fn repeat(this: Value, n: Value) -> Result<Value, Trace> {
    let string = this.into_string()?;
    let new_string = string.repeat(n.into_number()? as usize);

    Ok(new_string.to_value())
}

fn to_lowercase(this: Value) -> Result<Value, Trace> {
    Ok(this.into_string()?.to_lowercase().to_value())
}

fn to_uppercase(this: Value) -> Result<Value, Trace> {
    Ok(this.into_string()?.to_uppercase().to_value())
}

fn trim(this: Value) -> Result<Value, Trace> {
    Ok(this.into_string()?.trim().to_value())
}

fn trim_end(this: Value) -> Result<Value, Trace> {
    Ok(this.into_string()?.trim_end().to_value())
}

fn trim_start(this: Value) -> Result<Value, Trace> {
    Ok(this.into_string()?.trim_start().to_value())
}

fn ends_with(this: Value, s: Value) -> Result<Value, Trace> {
    Ok(this
        .into_string()?
        .ends_with(s.into_string()?.as_str())
        .to_value())
}

fn starts_with(this: Value, s: Value) -> Result<Value, Trace> {
    Ok(this
        .into_string()?
        .starts_with(s.into_string()?.as_str())
        .to_value())
}

fn split(this: Value, s: Value) -> Result<Value, Trace> {
    use std::{cell::RefCell, rc::Rc};

    let res: Vec<Value> = this
        .into_string()?
        .split(s.into_string()?.as_str())
        .map(|v| v.to_value())
        .collect();

    Ok(Value::Array(Rc::new(RefCell::new(res))))
}

pub fn register() -> Class {
    ClassBuilder::new("String")
        .add_method("len", len)
        .add_method("repeat", repeat)
        .add_method("is_empty", is_empty)
        .add_method("to_lowercase", to_lowercase)
        .add_method("to_uppercase", to_uppercase)
        .add_method("trim", trim)
        .add_method("trim_end", trim_end)
        .add_method("trim_start", trim_start)
        .add_method("ends_with", ends_with)
        .add_method("starts_with", starts_with)
        .add_method("split", split)
        .build()
}
