use radish_lang::{RadishConfig, Radish, Cli, RadishError};

fn main() -> Result<(), RadishError> {
    let args = Cli::new();

    let config = RadishConfig::from_cli(&args);

    let mut radish = Radish::with_settings(config);

    let source = radish.read_file(&args.path)?;

    if let Err(err) = radish.run_from_source(source) {        
        err.emit();
    } 



    Ok(())
}
