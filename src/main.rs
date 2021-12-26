mod ast;
mod compiler;
mod opcode;
mod parser;
mod scanner;
mod token;
mod value;
mod vm;

use parser::Parser;
use compiler::Compiler;
use vm::VM;

fn main() {
    println!("Hello, Radish!");

    let result = Parser::new("12 + 7 - 8 / 4 * 5").parse().unwrap();
    println!("{:#?}", result);

    let mut compiler = Compiler::new();
    compiler.run(&result);
    println!("{:?}", compiler.chunk);

    let mut vm = VM::new(compiler.chunk);
    vm.interpret();
}