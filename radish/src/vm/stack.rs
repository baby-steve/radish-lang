use crate::common::Value;

#[derive(Debug, PartialEq)]
pub struct Stack {
    pub stack: Vec<Value>,
}

impl Stack {
    pub fn new() -> Stack {
        Stack { stack: vec![] }
    }

    #[inline]
    pub fn push(&mut self, val: Value) {
        self.stack.push(val);
    }

    #[inline]
    pub fn pop(&mut self) -> Value {
        self.stack.pop().expect("stack should not be empty")
    }

    #[inline]
    pub fn peek(&mut self) -> Option<Value> {
        if self.stack.is_empty() {
            None
        } else {
            Some(self.stack[self.stack.len() - 1].clone())
        }
    }

    #[inline]
    pub fn peek_n(&mut self, index: usize) -> Value {
        self.stack[self.stack.len() - index].clone()
    }

    #[inline]
    pub fn get(&self, index: usize) -> &Value {
        &self.stack[index]
    }
}

impl Default for Stack {
    fn default() -> Self {
        Stack::new()
    }
}
