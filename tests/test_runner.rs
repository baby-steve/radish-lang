use radish_lang::{compiler::Compiler, parser::Parser, source::Source, vm::VM};
use std::fs;
use std::path::PathBuf;
use std::rc::Rc;

fn test_file(source: Rc<Source>) {
    let result = Parser::new(Rc::clone(&source)).parse();

    match result {
        Ok(res) => {
            let mut compiler = Compiler::new();
            compiler.run(&res);

            let mut vm = VM::new(compiler.chunk);
            vm.interpret();
        }
        Err(err) => {
            println!("{}", err);
            panic!("Test failed.");
        }
    };
}

fn test_files() {
    let paths =
        fs::read_dir("./tests/snippets").expect("You must be in base directory to run tests");

    let mut files: Vec<PathBuf> = vec![];

    for path in paths {
        files.push(path.expect("Could not read path").path());
    }

    println!("Running {:?} test file(s)...\n", &files.len());

    while let Some(path) = files.pop() {
        let file_contents = fs::read_to_string(&path).expect("Could not read file");
        let source = Source::new(&file_contents, &path);

        test_file(source);
    }
}

#[test]
fn end_to_end_snippets() {
    test_files();
}