use radish_lang::{Radish, Cli};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Cli::new();

    let mut radish = Radish::new();

    let source = radish.read_file(&args.path)?;

    radish.run_from_source(source);
    
    Ok(())
}
