use crate::common::{Chunk, Module};
use crate::vm::native::NativeFunction;
use crate::VM;
use std::cell::RefCell;
use std::cmp::{Ord, Ordering};
use std::collections::{hash_map, HashMap};
use std::fmt::{self};
use std::ops::{Add, Div, Mul, Neg, Not, Rem, Sub};
use std::rc::{Rc, Weak};

use super::CallFrame;
use super::stack::Stack;

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(Rc<RefCell<String>>),
    Function(Rc<Function>),
    Closure(Rc<Closure>),
    Class(Rc<Class>),
    Instance(Rc<Instance>),
    Module(Rc<RefCell<Module>>),
    //Map(Rc<RefCell<ValueMap>>),
    NativeFunction(Rc<NativeFunction>),
    Nil,
}

impl Value {
    #[inline]
    pub fn into_string(self) -> Result<Rc<RefCell<String>>, String> {
        match self {
            Value::String(s) => Ok(s),
            _ => Err("expected a string".to_string()),
        }
    }

    #[inline]
    pub fn into_function(self) -> Result<Rc<Function>, String> {
        match self {
            Value::Function(f) => Ok(f),
            _ => Err("expected a function".to_string()),
        }
    }

    #[inline]
    pub fn into_class(self) -> Result<Rc<Class>, String> {
        match self {
            Value::Class(class) => Ok(class),
            _ => Err("expected a class".to_string()),
        }
    }

    #[inline]
    pub fn into_closure(self) -> Result<Rc<Closure>, String> {
        match self {
            Value::Closure(closure) => Ok(closure),
            _ => Err("expected a closure".to_string()),
        }
    }

    #[inline]
    pub fn into_module(self) -> Result<Rc<RefCell<Module>>, String> {
        match self {
            Value::Module(module) => Ok(module),
            _ => Err("expected a module".to_string()),
        }
    }
}

impl From<f64> for Value {
    fn from(num: f64) -> Self {
        Value::Number(num)
    }
}

impl From<bool> for Value {
    fn from(val: bool) -> Self {
        Value::Boolean(val)
    }
}

impl From<&str> for Value {
    fn from(val: &str) -> Self {
        Value::String(Rc::new(RefCell::new(val.to_string())))
    }
}

impl From<&String> for Value {
    fn from(val: &String) -> Self {
        Value::String(Rc::new(RefCell::new(val.to_string())))
    }
}

impl From<Function> for Value {
    fn from(val: Function) -> Self {
        Value::Function(Rc::new(val))
    }
}

impl From<Class> for Value {
    fn from(class: Class) -> Self {
        Value::Class(Rc::new(class))
    }
}

impl Clone for Value {
    fn clone(&self) -> Value {
        match self {
            Self::Closure(val) => Self::Closure(Rc::clone(val)),
            Self::Function(val) => Self::Function(Rc::clone(val)),
            Self::Nil => Self::Nil,
            Self::Boolean(val) => Self::Boolean(*val),
            Self::Number(val) => Self::Number(*val),
            Self::String(val) => Self::String(Rc::clone(val)),
            Self::Class(val) => Self::Class(Rc::clone(val)),
            Self::Instance(inst) => Self::Instance(Rc::clone(inst)),
            Self::Module(module) => Self::Module(Rc::clone(module)),
            Self::NativeFunction(val) => Self::NativeFunction(Rc::clone(val)),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(num) => f.write_str(&num.to_string()),
            Value::Boolean(false) => f.write_str("false"),
            Value::Boolean(true) => f.write_str("true"),
            Value::String(val) => f.write_str(&format!("\"{}\"", val.borrow())),
            Value::Function(val) => write!(f, "<fun {}>", val.format_name()),
            Value::Closure(val) => write!(f, "<fun {}>", val.function.format_name()),
            Value::Class(val) => write!(f, "<class {}>", val.name.borrow()),
            Value::Instance(val) => write!(f, "<{:?} instance>", val.class.name.borrow()),
            Value::Module(module) => write!(f, "<mod {}>", module.borrow().name),
            Value::NativeFunction(_) => write!(f, "<native fun>"),
            //Value::Map(map) => write!(f, "{:?}", map.borrow().to_string()),
            Value::Nil => f.write_str("nil"),
        }
    }
}

impl Add for Value {
    type Output = Self;
    fn add(self, other: Value) -> <Self as std::ops::Add<Value>>::Output {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a + b),
            (Value::String(a), Value::String(b)) => {
                // FIXME: I believe this doesn't work. Need to look into it.
                a.borrow_mut().push_str(&b.borrow());
                Value::String(a)
            }
            _ => panic!("Operands must be numbers"),
        }
    }
}

impl Sub for Value {
    type Output = Self;
    fn sub(self, other: Value) -> <Self as std::ops::Sub<Value>>::Output {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a - b),
            _ => panic!("Operands must be numbers"),
        }
    }
}

impl Mul for Value {
    type Output = Self;
    fn mul(self, other: Value) -> <Self as std::ops::Mul<Value>>::Output {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a * b),
            _ => panic!("Operands must be numbers"),
        }
    }
}

impl Div for Value {
    type Output = Self;
    fn div(self, other: Value) -> <Self as std::ops::Div<Value>>::Output {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a / b),
            _ => panic!("Operands must be numbers"),
        }
    }
}

impl Rem for Value {
    type Output = Self;
    fn rem(self, other: Value) -> <Self as std::ops::Rem<Value>>::Output {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a % b),
            _ => panic!("Operands must be numbers"),
        }
    }
}

impl Neg for Value {
    type Output = Self;
    fn neg(self) -> Self::Output {
        match self {
            Value::Number(val) => Value::Number(-val),
            _ => panic!("Operands must be numbers"),
        }
    }
}

impl Not for Value {
    type Output = Self;
    fn not(self) -> Self::Output {
        match self {
            Value::Boolean(val) => Value::Boolean(!val),
            _ => panic!("Operand must be boolean"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: Box<str>,
    pub module: Weak<RefCell<Module>>,
}

impl Function {
    pub fn format_name(&self) -> &str {
        if self.name.is_empty() {
            "script"
        } else {
            &*self.name
        }
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl PartialOrd for Function {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Function {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(&other.name)
    }
}

impl Eq for Function {}

/// Runtime representation of a closure.
#[derive(Debug)]
pub struct Closure {
    /// This closure's function.
    pub function: Rc<Function>,
    /// Store 'non-locals'.
    pub non_locals: RefCell<Vec<UpValue>>,
}

impl Closure {
    pub fn new(function: Rc<Function>) -> Self {
        Self {
            function,
            non_locals: RefCell::new(vec![]),
        }
    }
}

impl PartialEq for Closure {
    fn eq(&self, other: &Self) -> bool {
        self.function.name == other.function.name
    }
}

impl PartialOrd for Closure {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Closure {
    fn cmp(&self, other: &Self) -> Ordering {
        self.function.name.cmp(&other.function.name)
    }
}

impl Eq for Closure {}

impl From<Rc<Function>> for Closure {
    fn from(function: Rc<Function>) -> Closure {
        Closure::new(function)
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum UpValueLocation {
    StackIndex(usize),
    UpValueIndex(usize),
}

/// Runtime representation of an up-value.
#[derive(Debug, PartialEq, PartialOrd)]
pub struct UpValue {
    /// The place where the value is stored after its closed over.
    closed: Option<Value>,
    /// The Value's location on the stack before its closed over.
    location: Option<UpValueLocation>,
}

impl UpValue {
    pub fn new(pos: usize, on_stack: bool) -> Self {
        let location = if on_stack {
            UpValueLocation::StackIndex(pos)
        } else {
            UpValueLocation::UpValueIndex(pos)
        };

        Self {
            closed: None,
            location: Some(location),
        }
    }

    /// Get this upvalue's inner value.
    pub fn inner(&self, vm: &VM) -> Value {
        if let Some(val) = &self.closed {
            return val.clone();
        } else if let Some(index) = &self.location {
            let val = match index {
                UpValueLocation::StackIndex(idx) => vm.stack.get(*idx).clone(),
                UpValueLocation::UpValueIndex(idx) => {
                    let prev_frame = &vm.frames[vm.frame_count - 1].closure;

                    println!("[vm] previous frame name: {}", prev_frame.function.name);

                    let upval = &vm.frames[vm.frame_count - 1].closure.non_locals.borrow()[*idx];

                    let val = upval.inner(vm);
                    val
                }
            };

            val
        } else {
            unreachable!("Can't get inner value: Upvalue doesn't have a location or a value");
        }
    }

    /// Set this upvalue's inner value.
    pub fn set_value(&mut self, frames: &Vec<CallFrame>, stack: &mut Stack, new_val: Value) {
        if let Some(val) = &mut self.closed {
            *val = new_val;
        } else if let Some(location) = &self.location {
            match location {
                UpValueLocation::StackIndex(idx) => {
                    stack.stack[*idx] = new_val;
                },
                UpValueLocation::UpValueIndex(idx) => {
                    let frame_count = frames.len();
                    let prev_frame = &frames[frame_count - 1].closure;

                    prev_frame.non_locals.borrow_mut()[*idx].set_value(frames, stack, new_val);
                },
            }
        } else {
            unreachable!("Can't set new value: Upvalue doesn't have a location or a value");
        }
    }

    /// Close up this upvalue.
    pub fn close(&mut self, vm: &VM) {
        let location = self
            .location
            .take()
            .expect("Upvalue doesn't have a location. It may have already been closed over.");

        let val = match location {
            UpValueLocation::StackIndex(idx) => vm.stack.stack[idx].clone(),
            UpValueLocation::UpValueIndex(idx) => {
                let prev_frame = &vm.frames[vm.frame_count - 1].closure;

                prev_frame.non_locals.borrow()[idx].inner(vm).clone()
            }
        };

        self.closed = Some(val);
    }
}

#[derive(Debug)]
pub struct Class {
    /// the class name
    // TODO:
    // this doesn't need to be wrapped in a reference counter and refcell
    // its only so that it matches the type of Value::String.
    pub name: Rc<RefCell<String>>,
    pub constructors: RefCell<HashMap<String, Value>>,
}

impl Class {
    pub fn new(name: &Rc<RefCell<String>>) -> Self {
        Class {
            name: Rc::clone(name),
            constructors: RefCell::new(HashMap::new()),
        }
    }
}

impl PartialEq for Class {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl PartialOrd for Class {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Class {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(&other.name)
    }
}

impl Eq for Class {}
/*
#[derive(Debug, PartialEq, PartialOrd)]
pub enum MethodType {
    Method,
    Constructor,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Method {
    pub ty: MethodType,
    pub receiver: Instance,
    pub method: Rc<Closure>,
}*/

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Instance {
    // A reference to the class this is an instance of.
    class: Rc<Class>,
    // This instance's fields.
    fields: Vec<Value>,
}

impl Instance {
    pub fn new(class: &Rc<Class>) -> Self {
        Instance {
            class: Rc::clone(class),
            // TODO:
            // do we know how many fields an instance has?
            // if so we could instead do `Vec::with_capacity(num_fields)`
            fields: Vec::new(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ValueMap {
    inner: HashMap<String, Value>,
}

impl ValueMap {
    pub fn iter(&self) -> hash_map::Iter<String, Value> {
        self.inner.iter()
    }
}

impl PartialOrd for ValueMap {
    fn partial_cmp(&self, _other: &Self) -> Option<Ordering> {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::Value;

    #[test]
    fn test_size() {
        assert_eq! {
            std::mem::size_of::<Value>(),
            16,
        };
    }
}
