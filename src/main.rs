use radish_lang::common::disassembler::Disassembler;
use radish_lang::common::source::Source;
use radish_lang::compiler::{analysis::Analyzer, compiler::Compiler, parser::Parser};
use radish_lang::vm::vm::{RadishConfig, VM};

use std::path::PathBuf;

fn main() {
    println!("Hello, Radish!");

    // Temporary code to pipe everything together.
    // Eventually will create pipeline or api or something.
    let source = Source::new(
        "
        print \"Hello, World!\"

        var i = 0
        while i <= 10 loop
            if i == 5 or i == 10 then 
                print \"ok\"
            else 
                print i
            endif
            i += 1
        endloop
        ",
        &PathBuf::from("./test_file"),
    );

    let result = Parser::new(source.clone()).parse();
    println!("{:#?}", result);

    match result {
        Ok(res) => {
            let config = RadishConfig::new();

            let mut semantic_analyzer = Analyzer::new();
            semantic_analyzer.analyze(&res);
            
            let mut compiler = Compiler::new();
            compiler.run(&res);

            Disassembler::disassemble_chunk("script", &compiler.chunk);

            let mut vm = VM::new(&config);
            vm.interpret(compiler.chunk);
            println!("{:?}", vm.globals);
        }
        Err(err) => println!("{}", err),
    }
}
