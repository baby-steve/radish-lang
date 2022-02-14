use crate::common::value::Value;

#[derive(Debug, PartialEq)]
pub struct Stack {
    pub stack: Vec<Value>,
}

impl Stack {
    pub fn new() -> Stack {
        Stack { stack: vec![] }
    }

    pub fn push(&mut self, val: Value) {
        //dbg!("did it get to here?");
        self.stack.push(val);
    }

    pub fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }

    pub fn peek(&mut self) -> Option<Value> {
        if self.stack.is_empty() {
            None
        } else {
            Some(self.stack[self.stack.len() - 1].clone())
        }
    }

    pub fn peek_n(&mut self, index: usize) -> Option<Value> {
        if self.stack.len() < index {
            None
        } else {
            Some(self.stack[self.stack.len() - index].clone())
        }
    }
}