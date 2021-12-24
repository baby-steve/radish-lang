mod ast;
mod opcode;
mod parser;
mod scanner;
mod token;
mod value;
mod vm;

use opcode::Opcode;
use parser::Parser;

fn main() {
    println!("Hello, Radish!");

    let result = Parser::new("12 + 3 - 45 * 678 / 9").parse();
    println!("{:?}", result);
}
