mod scanner;
mod token;
mod ast;
mod parser;

use parser::Parser;

fn main() {
    println!("Hello, Radish!");

    let result = Parser::new("").parse();
    println!("{:?}", result);
}
