use std::{cell::RefCell, cmp::Ordering, collections::HashMap};

use crate::Value;

use super::immutable_string::ImmutableString;

#[derive(Debug)]
pub struct Class {
    name: ImmutableString,
    pub items: RefCell<HashMap<ImmutableString, ClassItem>>,
}

impl Class {
    pub fn new<N: Into<ImmutableString>>(name: N) -> Self {
        Self {
            name: name.into(),
            items: RefCell::new(HashMap::new()),
        }
    }

    pub fn add_field<N>(&self, name: N, field: Value, access_typ: AccessType)
    where
        N: Into<ImmutableString>,
    {
        let item = ClassItem::new(field, access_typ, ClassItemType::Field);

        self.items.borrow_mut().insert(name.into(), item);
    }

    pub fn add_method<N>(&self, name: N, fun: Value, access_typ: AccessType)
    where
        N: Into<ImmutableString>,
    {
        let item = ClassItem::new(fun, access_typ, ClassItemType::Method);

        self.items.borrow_mut().insert(name.into(), item);
    }

    pub fn add_constructor<N>(&self, name: N, fun: Value)
    where
        N: Into<ImmutableString>,
    {
        let item = ClassItem::new(fun, AccessType::Public, ClassItemType::Method);

        self.items.borrow_mut().insert(name.into(), item);
    }

    pub fn name(&self) -> &str {
        &self.name
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

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ClassItem {
    value: Value,
    access_typ: AccessType,
    item_typ: ClassItemType,
}

impl ClassItem {
    pub fn new(field: Value, access_typ: AccessType, item_typ: ClassItemType) -> Self {
        Self {
            value: field,
            access_typ,
            item_typ,
        }
    }

    pub fn is_private(&self) -> bool {
        match self.access_typ {
            AccessType::Private => true,
            _ => false,
        }
    }

    pub fn inner(&self) -> Value {
        self.value.clone()
    }

    pub fn set_inner(&mut self, value: Value) {
        self.value = value;
    }

    pub fn typ(&self) -> ClassItemType {
        self.item_typ
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum AccessType {
    Public,
    Private,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ClassItemType {
    Field,
    Method,
    Constructor,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_field() {
        let class = Class::new("Test");

        class.add_field("a", Value::Nil, AccessType::Private);

        let items = class.items.borrow();
        let field = items.get("a");

        assert!(field.is_some());
    }
}