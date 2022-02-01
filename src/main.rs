use radish_lang::common::disassembler::Disassembler;
use radish_lang::common::source::Source;
use radish_lang::compiler::{
    analysis::Analyzer, compiler::Compiler, parser::Parser,
};
use radish_lang::vm::vm::{RadishConfig, VM};

use std::path::PathBuf;

fn main() {
    println!("Hello, Radish!");

    // Temporary code to pipe everything together.
    // Eventually will create pipeline or api or something.
    let source = Source::new(
        "
        fun main(a, b) {
            var i = 0
            while i < a loop
                i += 1
                if i == b then
                    print \"a is equal to b\"
                else
                    print i
                endif
            endloop
        }

        main(13, 8)
        ",
        &PathBuf::from("./test_file"),
    );

    let result = Parser::new(source.clone()).parse();
    println!("{:#?}", result);

    match result {
        Ok(res) => {
            let config = RadishConfig::new();

            let mut semantic_analyzer = Analyzer::new();
            let sym_table = semantic_analyzer.analyze(&res);

            let mut compiler = Compiler::new(&sym_table);
            let script = compiler.compile(&res);

            Disassembler::disassemble_chunk("script", &script.chunk);

            let mut vm = VM::new(&config);
            vm.interpret(script);

            println!("\nOkay we done now.");
        }
        Err(err) => println!("{}", err),
    }
}
