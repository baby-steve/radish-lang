use std::hash::Hash;
use std::rc::Rc;
use std::ops::Deref;

#[derive(Debug, Clone, Default, Hash, PartialOrd, Ord, Eq)]
pub struct ImmutableString(Rc<String>);

impl Deref for ImmutableString {
    type Target = String;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl AsRef<str> for ImmutableString {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl PartialEq for ImmutableString {
    fn eq(&self, other: &Self) -> bool {
        &self.0.len() == &other.0.len() && &self.0 == &other.0
    }
}

impl From<String> for ImmutableString {
    fn from(s: String) -> Self {
        Self(Rc::new(s))
    }
}

impl From<&String> for ImmutableString {
    fn from(s: &String) -> Self {
        Self(Rc::new(s.to_string()))
    }
}

impl From<&str> for ImmutableString {
    fn from(s: &str) -> Self {
        Self(Rc::new(s.to_string()))
    }
}

impl From<&mut str> for ImmutableString {
    fn from(s: &mut str) -> Self {
        Self(Rc::new(s.to_string()))
    }
}

impl From<Box<str>> for ImmutableString {
    fn from(s: Box<str>) -> Self {
        Self(Rc::new(s.into()))
    }
}

impl std::fmt::Display for ImmutableString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
