use radish::{RadishConfig, Radish, RadishError};

use clap::{App, Arg};

#[derive(Debug)]
pub struct Cli {
    pub path: String,
    pub args: Vec<String>,
    pub dump_ast: bool,
    pub dump_code: bool,
    pub trace: bool,
}

impl Cli {
    pub fn new() -> Cli {
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
                Arg::with_name("FILE.rdsh")
                    .help("Path to file")
                    .required(true),
            )
            .arg(
                Arg::with_name("arguments")
                    .help("Arguments passed to program")
                    .multiple(true),
            );

        let matches = app.get_matches();

        let path = matches
            .value_of("FILE.rdsh")
            .expect("Path can't possibly be None as it is required");

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
            path: path.to_string(),
            args,
            dump_ast,
            dump_code,
            trace,
        }
    }
}

fn main() -> Result<(), RadishError> {
    let args = Cli::new();

    let config = RadishConfig {
        dump_ast: args.dump_ast,
        dump_code: args.dump_code,
        trace: args.trace,
        ..RadishConfig::default()
    };

    let mut radish = Radish::with_settings(std::rc::Rc::new(config));

    let source = radish.read_file(&args.path)?;

    if let Err(err) = radish.run_from_source(source) {        
        err.emit();
    } 

    Ok(())
}
