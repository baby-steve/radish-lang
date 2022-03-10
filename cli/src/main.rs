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
        let mut radish = Radish::with_settings(std::rc::Rc::new(config));

        let module = radish.compile_file(&path)?;

        radish.run_module(module)?;
    } else {
        config.set_repl(true);

        let radish = Radish::with_settings(std::rc::Rc::new(config));
        
        repl::Repl::new(radish).run()?;
    }

    Ok(())
}
