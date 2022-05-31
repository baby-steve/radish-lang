use radish::{VM, RadishError};

fn main() -> Result<(), RadishError> {
    let mut vm = VM::new();

    let script = "print \"Hello, World!\"";

    vm.exec(script);

    Ok(())
}