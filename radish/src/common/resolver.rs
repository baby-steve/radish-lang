use std::path::PathBuf;

use crate::RadishError;

pub const RADISH_FILE_EXTENSION: &str = "rdsh";

#[derive(Debug)]
pub struct Resolver {
    extension: String,
}

impl Default for Resolver {
    fn default() -> Self {
        Resolver::new()
    }
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            extension: RADISH_FILE_EXTENSION.to_string(),
        }
    }

    pub fn resolve(&mut self, path: &str) -> Result<String, RadishError> {
        let mut file_path = PathBuf::from(path);

        match file_path.components().nth(0).unwrap() {
            std::path::Component::Normal(_) => Ok(path.to_string()),
            _ => {
                file_path.set_extension(&self.extension);
                Ok(file_path.to_string_lossy().into())
            }
        }
    }
}
