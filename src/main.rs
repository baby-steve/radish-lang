mod ast;
mod opcode;
mod parser;
mod scanner;
mod token;
mod value;
mod vm;

use parser::Parser;

fn main() {
    println!("Hello, Radish!");

    let result = Parser::new("1 + 23 - 4 * 56 / 7").parse();
    println!("{:#?}", result);
}
