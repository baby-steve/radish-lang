use radish::{VM, RadishError, Config};

mod cli;
mod repl;
mod hint;

fn main() -> Result<(), RadishError> {
    let args = cli::Cli::new();

    let mut config = Config::from(&args);

    if let Some(path) = args.path {

        let mut vm = VM::with_config(config);

        match vm.exec_file(&path) {
            Ok(_) => {}
            Err(err) => err.emit(),
        };
    } else {
        config.repl = true;

        let vm = VM::with_config(config);
        
        repl::Repl::new(vm).run();
    }

    Ok(())
}
