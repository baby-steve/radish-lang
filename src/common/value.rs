use std::cell::RefCell;
use std::cmp::{Ord, Ordering};
use std::collections::HashMap;
use std::fmt;
use std::ops::{Add, Div, Mul, Neg, Not, Sub};
use std::rc::{Rc, Weak};

use crate::common::chunk::Chunk;

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(Rc<RefCell<String>>),
    //Function(Rc<RefCell<Function>>),
    Function(Rc<Function>),
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
            Self::String(val) => Self::String(val.clone()),
            Self::Function(val) => Self::Function(val.clone()),
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
            Value::Function(val) => f.write_str(&format!("<fun {}>", val.name /*val.borrow().name*/)),
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

#[derive(Debug)]
pub struct Function {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: Box<str>,
    pub module: Weak<RefCell<Module>>,
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
pub struct Module {
    pub name: Box<str>,
    pub variables: Vec<Value>,
    pub symbols: HashMap<String, usize>,
}

impl PartialEq for Module {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl PartialOrd for Module {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Module {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(&other.name)
    }
}

impl Eq for Module {}

impl Module {
    pub fn new(name: &str) -> Rc<RefCell<Module>> {
        let module = Module {
            name: name.to_string().into_boxed_str(),
            variables: Vec::new(),
            symbols: HashMap::new(),
        };
        Rc::new(RefCell::new(module))
    }

    // add a variable
    // used by the compiler
    #[inline]
    pub fn add_var(&mut self, name: String) -> usize {
        let index = self.variables.len();
        self.variables.push(Value::Nil);

        self.symbols.insert(name, index);

        index
    }

    // get a symbol's index
    // used by the compiler
    #[inline]
    pub fn get_index(&self, name: &str) -> Option<usize> {
        // println!("{:?}", self.symbols);
        if let Some(index) = self.symbols.get(name) {
            Some(*index)
        } else {
            None
        }
    }

    // set a the variable at index to value
    // used by the vm
    #[inline]
    pub fn set_var(&mut self, index: usize, value: Value) {
        self.variables[index] = value;
    }

    // get a symbol's value
    // used by the vm
    #[inline]
    pub fn get_var(&self, index: usize) -> &Value {
        &self.variables[index]
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{:?}", self.variables)?;

        for var in &self.variables {
            write!(f, "[ {} ]", var)?;
        }

        writeln!(f, "")
    }
}
