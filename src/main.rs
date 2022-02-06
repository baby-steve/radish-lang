use radish_lang::common::disassembler::Disassembler;
use radish_lang::common::source::Source;
use radish_lang::compiler::{
    analysis::Analyzer, compiler::Compiler, parser::Parser, table::SymbolTable,
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

    let config = RadishConfig::new();
    let scope = SymbolTable::new(0);
    let mut parser = Parser::new(source.clone());
    let mut semantic_analyzer = Analyzer::new();
    let mut compiler = Compiler::new(&scope);
    let mut vm = VM::new(&config);

    let ast = match parser.parse() {
        Ok(result) => {
            println!("{:#?}", result);
            result
        }
        Err(err) => panic!("error: could not parse"),
    };

    let table = match semantic_analyzer.analyze(&ast) {
        Ok(table) => table,
        Err(()) => panic!("error: could not compile"),
    };

    let (module, script) = match compiler.compile(&ast, &table) {
        Ok((module, script)) => (module, script),
        Err(_) => panic!("error: could not compile"),
    };

    Disassembler::disassemble_chunk("script", &script);

    vm.interpret(script, module);

    println!("\nOkay we done now.");
}
