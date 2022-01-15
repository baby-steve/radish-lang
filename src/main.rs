use radish_lang::compiler::{
    parser::Parser,
    compiler::Compiler,
};
use radish_lang::common::source::Source;
use radish_lang::vm::VM;

use std::path::PathBuf;

fn main() {
    println!("Hello, Radish!");

    // Temporary code to pipe everything together.
    // Eventually will create pipeline or api or something.
    let source = Source::new(
        "var a = (20 + 4) - 10 >= 6 * 2 /\n (4 - \n 88) // this is a comment
         var b = 23
         b = b - 3
         print b * 2",
        &PathBuf::from("./test_file"),
    );
    let result = Parser::new(source.clone()).parse();

    match result {
        Ok(res) => {
            let mut compiler = Compiler::new();
            compiler.run(&res);
            println!("{:?}", compiler.chunk.code);
            let mut vm = VM::new(compiler.chunk);
            vm.interpret();
            println!("{:?}", vm.globals);
        }
        Err(err) => println!("{}", err),
    }
}