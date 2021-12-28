use radish_lang::compiler::Compiler;
use radish_lang::parser::Parser;
use radish_lang::vm::VM;
use radish_lang::source::Source;

fn main() {
    println!("Hello, Radish!");

    let source = Source::source("-1 + 2 - 3 * (1 + 2) / 2");
    let result = Parser::new(source.clone()).parse().unwrap();
    println!("{:#?}", result);

    let mut compiler = Compiler::new();
    compiler.run(&result);
    println!("{:?}", compiler.chunk);

    let mut vm = VM::new(compiler.chunk);
    vm.interpret();
}