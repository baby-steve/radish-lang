const DEFAULT_FILE_NAME: &str = "EVAL";

#[derive(Debug)]
pub struct Config {
    pub repl: bool,
    pub dump_ast: bool,
    pub dump_bytecode: bool,
    pub trace: bool,
    pub default_filename: String,
}

impl Default for Config {
    fn default() -> Self {
        Config::new()
    }
}

impl Config {
    pub fn new() -> Self {
        Self {
            repl: false,
            dump_ast: false,
            dump_bytecode: false,
            trace: false,
            default_filename: DEFAULT_FILE_NAME.to_string(),
        }
    }
}
