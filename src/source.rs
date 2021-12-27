use std::path::PathBuf;
use std::rc::Rc;

#[derive(Debug)]
pub struct Source {
    pub contents: String,
    pub path: PathBuf,
}

impl Source {
    pub fn new(source: String, path: PathBuf) -> Rc<Source> {
        Rc::new(Source {
            contents: source.to_string(),
            path: path.to_owned(),
        })
    }

    pub fn source(source: String) -> Rc<Source> {
        Rc::new(Source {
            contents: source.to_string(),
            path: PathBuf::default(), // should a source without a file have some default name?
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_new() {
        let source = Source::new("Hello, Radish!".to_string(), PathBuf::from("file/path"));
        assert_eq!(source.contents, "Hello, Radish!");
    }
}
