mod scanner;
mod token;
mod ast;
mod parser;

use parser::Parser;

fn main() {
    println!("Hello, Radish!");

    let result = Parser::new("12 + 3 - 45 * 678 / 9").parse();
    println!("{:?}", result);
}
