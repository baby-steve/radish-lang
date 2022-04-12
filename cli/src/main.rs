use std::path::{Path, PathBuf};

use radish::{Radish, RadishConfig, RadishError};

mod cli;
mod repl;
mod hint;

fn main() -> Result<(), RadishError> {
    let args = cli::Cli::new();

    let mut config = RadishConfig {
        dump_ast: args.dump_ast,
        dump_code: args.dump_code,
        trace: args.trace,
        ..RadishConfig::default()
    };

    if let Some(path) = args.path {
        // TODO: not the most elegant way to get the directory the entry file is in.
        config.set_source(&PathBuf::from(&path).parent().unwrap_or(&Path::new("")).to_str().unwrap());

        let mut radish = Radish::with_settings(std::rc::Rc::new(config));

        let module = match radish.compile_file(&path) {
            Ok(mod_) => mod_,
            Err(err) => {
                err.emit();
                return Err(err);
            }
        };

        if let Err(err) = radish.run_module(module) {
            err.emit();
            return Err(err);
        }
    } else {
        let source = std::env::current_dir()?;

        dbg!(&source);

        // TODO: not the most elegant way to get the directory the entry file is in.
        config.set_source(source.to_string_lossy());


        config.set_repl(true);

        let radish = Radish::with_settings(std::rc::Rc::new(config));
        
        repl::Repl::new(radish).run()?;
    }

    Ok(())
}
