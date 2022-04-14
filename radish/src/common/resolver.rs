use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use crate::compiler::pipeline::CompilerPipeLine;
use crate::RadishError;

use super::{CompiledModule, Module};

pub trait Resolver {
    fn resolve(
        &mut self,
        source: Option<impl AsRef<str>>,
        path: &str,
    ) -> Result<CompiledModule, RadishError>;
}

pub const RADISH_FILE_EXTENSION: &str = "rdsh";

#[derive(Debug)]
pub struct FileResolver {
    extension: String,
    base_path: Option<PathBuf>,
    cache: BTreeMap<PathBuf, CompiledModule>,
    stat: BTreeMap<String, CompiledModule>,
}

impl FileResolver {
    pub fn new() -> Self {
        Self {
            extension: RADISH_FILE_EXTENSION.to_string(),
            base_path: None,
            cache: BTreeMap::new(),
            stat: BTreeMap::new(),
        }
    }

    pub fn new_with_path(path: impl Into<PathBuf>) -> Self {
        Self {
            extension: RADISH_FILE_EXTENSION.to_string(),
            base_path: Some(path.into()),
            cache: BTreeMap::new(),
            stat: BTreeMap::new(),
        }
    }

    pub fn get_base_path(&self) -> Option<&Path> {
        self.base_path.as_ref().map(PathBuf::as_ref)
    }

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
        let path = Path::new(path);

        let mut file_path;

        if path.is_relative() {
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

        file_path
    }

    pub fn resolve(
        &mut self,
        source: Option<&str>,
        path: &str,
        pipeline: &mut CompilerPipeLine,
    ) -> Result<CompiledModule, RadishError> {
        if self.is_logical(path) {
            self.resolve_static(path)
        } else {
            self.resolve_file(source, path, pipeline)
        }
    }

    fn resolve_file(
        &mut self,
        source: Option<&str>,
        path: &str,
        pipeline: &mut CompilerPipeLine,
    ) -> Result<CompiledModule, RadishError> {
        dbg!(&path);

        let file_path = self.get_file_path(path.as_ref(), source);

        if let Some(module) = self.cache.get(&file_path) {
            return Ok(module.clone());
        }

        let src = self.load_file(&file_path)?;

        let file_name = file_path
            .file_name()
            .expect("expected a file name, found a directory")
            .to_str()
            .expect("invaild unicode in path name");

        let module = pipeline.compile(&file_name, &src)?;

        self.cache.insert(file_path, module.clone());

        Ok(module)
    }

    fn resolve_static(&mut self, path: &str) -> Result<CompiledModule, RadishError> {
        let module = self
            .stat
            .get(path)
            .expect("resolver does not have a module with the given path");

        Ok(std::rc::Rc::clone(module))
    }

    fn load_file(&self, file_path: &PathBuf) -> Result<String, RadishError> {
        let src = fs::read_to_string(file_path)?;
        Ok(src)
    }

    fn is_logical(&self, file_path: &str) -> bool {
        file_path.starts_with("@")
    }

    pub fn load(&mut self, name: impl ToString, module: Module) -> &mut Self {
        self.stat
            .insert(name.to_string(), Rc::new(RefCell::new(module)));

        self
    }
}
