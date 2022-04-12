use std::path::{Path, PathBuf};

use std::collections::BTreeMap;
use std::rc::Rc;

use crate::{Radish, RadishConfig, RadishError};

use super::CompiledModule;

pub trait Resolver {
    fn resolve(
        &mut self,
        source: Option<impl AsRef<str>>,
        path: &str,
        config: Rc<RadishConfig>,
    ) -> Result<CompiledModule, RadishError>;
}

pub const RADISH_FILE_EXTENSION: &str = "rdsh";

#[derive(Debug)]
pub struct FileResolver {
    extension: String,
    base_path: Option<PathBuf>,
    cache: BTreeMap<PathBuf, CompiledModule>,
}

impl FileResolver {
    pub fn new() -> Self {
        Self {
            extension: RADISH_FILE_EXTENSION.to_string(),
            base_path: None,
            cache: BTreeMap::new(),
        }
    }

    pub fn new_with_path(path: impl Into<PathBuf>) -> Self {
        Self {
            extension: RADISH_FILE_EXTENSION.to_string(),
            base_path: Some(path.into()),
            cache: BTreeMap::new(),
        }
    }

    // TODO: inline?
    pub fn get_base_path(&self) -> Option<&Path> {
        self.base_path.as_ref().map(PathBuf::as_ref)
    }
    
    // TODO: inline?
    pub fn set_base_path(&mut self, path: impl Into<PathBuf>) {
        self.base_path = Some(path.into());
    }

    pub fn is_cached(&self, path: impl AsRef<str>, source_path: Option<impl AsRef<str>>) -> bool {
        let file_path = self.get_file_path(path.as_ref(), source_path);

        if !self.cache.is_empty() {
            self.cache.contains_key(&file_path)
        } else {
            false
        }
    }

    fn get_file_path(&self, path: &str, source_path: Option<impl AsRef<str>>) -> PathBuf {
        dbg!(&path);

        let path = Path::new(path);

        let mut file_path;

        if path.is_relative() {
            let msg: &str = &source_path.as_ref().clone().unwrap().as_ref();
            dbg!(msg);

            file_path = self
                .base_path
                .clone()
                .or_else(|| source_path.map(|p| p.as_ref().into()))
                .unwrap_or_default();
            file_path.push(path);
        } else {
            file_path = path.into();
        }

        file_path.set_extension(&self.extension);

        dbg!(&file_path);

        file_path
    }

    fn resolve_module(
        &mut self,
        path: &str,
        source: Option<impl AsRef<str>>,
        config: Rc<RadishConfig>,
    ) -> Result<CompiledModule, RadishError> {
        dbg!(&path);

        let file_path = self.get_file_path(path.as_ref(), source);

        if let Some(module) = self.cache.get(&file_path) {
            return Ok(module.clone());
        }

        // The following is something of a hack.

        let mut radish = Radish::with_settings(config);

        let module = radish.compile_file_with_path(&file_path)?;

        self.cache.insert(file_path, module.clone());

        Ok(module)
    }
}

impl Resolver for FileResolver {
    fn resolve(
        &mut self,
        source: Option<impl AsRef<str>>,
        path: &str,
        config: Rc<RadishConfig>,
    ) -> Result<CompiledModule, RadishError> {
        self.resolve_module(path, source, config)
    }
}
