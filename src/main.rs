use radish_lang::compiler::Compiler;
use radish_lang::parser::Parser;
use radish_lang::source::Source;
use radish_lang::vm::VM;

use std::path::PathBuf;

fn main() {
    println!("Hello, Radish!");

    // Temporary code to pipe everything together.
    // Eventually will create pipeline or api or something.
    let source = Source::new(
        "(20 + 4) - 10 >= 6 * 2 /\n (4 - \n 88) // this is a comment",
        &PathBuf::from("./test_file"),
    );
    let result = Parser::new(source.clone()).parse();

    match result {
        Ok(res) => {
            let mut compiler = Compiler::new();
            compiler.run(&res);

            let mut vm = VM::new(compiler.chunk);
            vm.interpret();
        }
        Err(err) => println!("{}", err),
    }
}