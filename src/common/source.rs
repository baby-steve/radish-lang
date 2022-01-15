use std::path::PathBuf;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Source {
    pub contents: String,
    pub path: PathBuf,
}

impl Source {
    pub fn new(source: &str, path: &PathBuf) -> Rc<Source> {
        Rc::new(Source {
            contents: source.to_string(),
            path: path.to_owned(),
        })
    }

    pub fn source(source: &str) -> Rc<Source> {
        Rc::new(Source {
            contents: source.to_string(),
            path: PathBuf::from("./main"), // should a source without a file have some default name?
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_new() {
        let source = Source::new("Hello, Radish!", &PathBuf::from("file/path"));
        assert_eq!(source.contents, "Hello, Radish!");
    }
}
