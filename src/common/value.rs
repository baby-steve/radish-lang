use std::cell::RefCell;
use std::cmp::{Ord, Ordering};
use std::fmt::{self};
use std::ops::{Add, Div, Mul, Neg, Not, Rem, Sub};
use std::rc::{Rc, Weak};

use crate::common::{Chunk, Module};

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(Rc<RefCell<String>>),
    Function(Rc<Function>),
    //Closure(Rc<Closure>),
    Closure(Closure),
    Nil,
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

impl From<Function> for Value {
    fn from(val: Function) -> Self {
        //Value::Function(Rc::new(RefCell::new(val)))
        Value::Function(Rc::new(val))
    }
}

impl Clone for Value {
    fn clone(&self) -> Value {
        match self {
            Self::Nil => Self::Nil,
            Self::Boolean(val) => Self::Boolean(*val),
            Self::Number(val) => Self::Number(*val),
            Self::String(val) => Self::String(Rc::clone(val)),
            Self::Function(val) => Self::Function(Rc::clone(val)),
            Self::Closure(val) => Self::Closure(Closure::from(Rc::clone(&val.function))),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(num) => f.write_str(&format!("{}", num.to_string())),
            Value::Boolean(false) => f.write_str("false"),
            Value::Boolean(true) => f.write_str("true"),
            Value::String(val) => f.write_str(&format!("\"{}\"", val.borrow())),
            Value::Function(val) => write!(f, "<fun {}>", val.format_name()),
            Value::Closure(val) => write!(f, "<fun {}>", val.function.format_name()),
            Value::Nil => f.write_str("nil"),
        }
    }
}

impl Add for Value {
    type Output = Self;
    fn add(self, other: Value) -> <Self as std::ops::Add<Value>>::Output {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => return Value::Number(a + b),
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
        if &*self.name == "" {
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

#[derive(Debug)]
pub struct Closure {
    pub function: Rc<Function>,
}

impl Clone for Closure {
    fn clone(&self) -> Closure {
        Closure {
            function: Rc::clone(&self.function),
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
        Closure { function }
    }
}
