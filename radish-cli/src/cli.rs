//! CLI for the Radish programming langauge.

use clap::{App, Arg};
use radish::Config;

#[derive(Debug)]
pub struct Cli {
    /// Path to the file to run. if one isn't given, REPL mode will be run instead.
    pub path: Option<String>,
    /// Any extra args passed into the CLI.
    pub args: Vec<String>,
    pub dump_ast: bool,
    pub dump_code: bool,
    pub trace: bool,
}

impl Cli {
    pub fn new() -> Self {
        let version = &format!("v{}", env!("CARGO_PKG_VERSION"))[..];

        let app = App::new("radish")
            .about("The Radish scripting language")
            .version(version)
            .arg(
                Arg::with_name("dump-ast")
                    .long("dump-ast")
                    .short("a")
                    .help("Dump the program's AST (Abstract Syntax Tree)"),
            )
            .arg(
                Arg::with_name("dump-bytecode")
                    .long("dump-bytecode")
                    .short("d")
                    .help("Dump the program's bytecode"),
            )
            .arg(
                Arg::with_name("trace")
                    .long("trace")
                    .short("t")
                    .help("Trace the VM's execution"),
            )
            .arg(
                Arg::with_name("FILE.rdsh").help("Path to file"), //.required(true),
            )
            .arg(
                Arg::with_name("arguments")
                    .help("Arguments passed to program")
                    .multiple(true),
            );

        let matches = app.get_matches();

        let path = if let Some(path) = matches.value_of("FILE.rdsh") {
            Some(path.to_string())
        } else {
            None
        };

        let dump_ast = matches.is_present("dump-ast");
        let dump_code = matches.is_present("dump-bytecode");
        let trace = matches.is_present("trace");

        let args = matches
            .values_of("arguments")
            .unwrap_or_default()
            .collect::<Vec<_>>()
            .iter()
            .map(|arg| arg.to_string())
            .collect();

        Cli {
            path,
            args,
            dump_ast,
            dump_code,
            trace,
        }
    }
}

impl From<&Cli> for Config {
    fn from(cli: &Cli) -> Self {
        Config {
            dump_ast: cli.dump_ast,
            dump_bytecode: cli.dump_code,
            trace: cli.trace,
            ..Default::default()
        }
    }
}
