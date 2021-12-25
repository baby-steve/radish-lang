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

fn main() {
    println!("Hello, Radish!");

    let result = Parser::new("1 + 23 - 4 * 56 / 7").parse().unwrap();
    println!("{:#?}", result);

    let mut compiler = Compiler::new();
    compiler.run(&result);
    println!("{:?}", compiler.chunk);
}
