use std::path::PathBuf;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq)]
pub struct Source {
    pub contents: String,
    pub path: PathBuf,
}

impl Source {
    pub fn new(source: &str, path: impl ToString) -> Rc<Source> {
        Rc::new(Source {
            contents: source.to_string(),
            path: PathBuf::from(path.to_string()),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_new() {
        let source = Source::new("Hello, Radish!", "file/path");
        assert_eq!(source.contents, "Hello, Radish!");
    }
}
