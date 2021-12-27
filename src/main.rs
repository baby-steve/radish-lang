use radish_lang::compiler::Compiler;
use radish_lang::parser::Parser;
use radish_lang::vm::VM;

fn main() {
    println!("Hello, Radish!");

    let result = Parser::new("-1 + 2 - 3 * (1 + 2) / 2").parse().unwrap();
    println!("{:#?}", result);

    let mut compiler = Compiler::new();
    compiler.run(&result);
    println!("{:?}", compiler.chunk);

    let mut vm = VM::new(compiler.chunk);
    vm.interpret();
}