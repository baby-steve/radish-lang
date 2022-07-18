use std::{cell::RefCell, cmp::Ordering, collections::HashMap, rc::Rc};

use crate::ToValue;

use super::{native::InnerMethod, ImmutableString, RegisterMethod, Value, NativeMethod};

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
        let item = ClassItem::new(fun, AccessType::Public, ClassItemType::Constructor);

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
    pub fn new(value: Value, access_typ: AccessType, item_typ: ClassItemType) -> Self {
        Self {
            value,
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

pub struct ClassBuilder {
    name: ImmutableString,
    fields: Vec<(ImmutableString, Value)>,
    methods: Vec<(ImmutableString, Rc<InnerMethod>)>,
}

impl ClassBuilder {
    pub fn new(name: impl Into<ImmutableString>) -> Self {
        Self {
            name: name.into(),
            fields: vec![],
            methods: vec![],
        }
    }

    pub fn add_method<N, M, A, R>(mut self, name: N, method: M) -> Self
    where
        N: Into<ImmutableString>,
        M: RegisterMethod<A, R>,
    {
        let name = name.into();
        let method = method.register();

        self.methods.push((name, method));

        self
    }

    pub fn add_field<N, V>(mut self, name: N, value: V) -> Self
    where
        N: Into<ImmutableString>,
        V: ToValue,
    {
        let name = name.into();
        let value = value.to_value();

        self.fields.push((name, value));

        self
    }

    pub fn build(self) -> Class {
        let class = Class::new(self.name);

        for (key, value) in self.fields {
            class.add_field(key, value, AccessType::Public);
        }

        for (key, value) in self.methods {
            let method = Rc::new(NativeMethod::new(value, Value::Nil));

            class.add_method(key, Value::NativeMethod(method), AccessType::Public);
        }

        class
    }
}

#[cfg(test)]
mod tests {
    use crate::Trace;

    use super::*;

    #[test]
    fn build_class() {
        fn test_method() -> Result<Value, Trace> {
            Ok(Value::Nil)
        }

        let class = ClassBuilder::new("Test")
            .add_field("a", 23.0f64)
            .add_method("b", test_method)
            .build();

        assert_eq!(
            class
                .items
                .borrow()
                .get(&ImmutableString::from("a"))
                .unwrap()
                .item_typ,
            ClassItemType::Field
        );

        assert_eq!(
            class
                .items
                .borrow()
                .get(&ImmutableString::from("b"))
                .unwrap()
                .item_typ,
            ClassItemType::Method
        );
    }
}
