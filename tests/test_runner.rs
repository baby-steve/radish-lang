//! Runs snippet tests.

use radish_lang::compiler::table::SymbolTable;
use radish_lang::{
    common::source::Source,
    compiler::{analysis::Analyzer, compiler::Compiler, parser::Parser},
    vm::vm::{RadishConfig, RadishFile, VM},
};

use std::{cell::RefCell, fs, path::PathBuf, rc::Rc};

use lazy_static::lazy_static;
use regex::Regex;

// A logger to capture the VM's output.
struct RadishMockLogger {
    buffer: RefCell<Vec<String>>,
}

impl RadishMockLogger {
    pub fn new() -> Rc<RadishMockLogger> {
        Rc::new(RadishMockLogger {
            buffer: RefCell::new(vec![]),
        })
    }
}

impl RadishFile for RadishMockLogger {
    fn write(&self, msg: &str) {
        self.buffer.borrow_mut().push(String::from(msg));
    }
}

struct TestSnippet {
    expected_values: Vec<String>,
    source: Rc<Source>,
}

impl TestSnippet {
    pub fn new(source: Rc<Source>) -> TestSnippet {
        lazy_static! {
            static ref EXPECTED: Regex = Regex::new("print (.*)// expect: ?(.*)").unwrap();
        }
        let lines = source.contents.lines();
        let mut expected = vec![];
        for line in lines {
            if EXPECTED.is_match(line) {
                let spliced = line.split("//").collect::<Vec<&str>>()[1].trim();
                let value = spliced[7..].trim().to_string();
                expected.push(value);
            }
        }

        TestSnippet {
            expected_values: expected,
            source: Rc::clone(&source),
        }
    }

    pub fn run(&self) {
        println!("testing {:?}...", &self.source.path);

        let result = Parser::new(Rc::clone(&self.source)).parse();

        match result {
            Ok(_) => {
                // Capture all messages printed from the VM.
                let stdout = RadishMockLogger::new();

                // Configuartion with stdout
                let config = RadishConfig::with_stdout(stdout.clone());

                let scope = SymbolTable::new(0);
                let mut parser = Parser::new(self.source.clone());
                let mut semantic_analyzer = Analyzer::new();
                let mut compiler = Compiler::new(&scope);
                let mut vm = VM::new(&config);

                let ast = match parser.parse() {
                    Ok(result) => {
                        println!("{:#?}", result);
                        result
                    }
                    Err(_) => panic!("error: could not parse"),
                };

                let table = match semantic_analyzer.analyze(&ast) {
                    Ok(table) => table,
                    Err(_) => panic!("error: could not compile"),
                };

                let (module, script) = match compiler.compile(&ast, &table) {
                    Ok((module, script)) => (module, script),
                    Err(_) => panic!("error: could not compile"),
                };

                vm.interpret(script, module);

                // Check each value printed by vm to their expected value.
                for (i, value) in stdout.buffer.borrow().iter().enumerate() {
                    let expected_value = &self.expected_values[i];

                    if expected_value != value {
                        println!("Expected '{}', but got '{}'", expected_value, value);
                        panic!("Test failed");
                    }
                }
            }
            Err(err) => {
                println!("{:?}", err);
                panic!("Test failed.");
            }
        };
    }
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

        TestSnippet::new(source).run();
    }
}

#[test]
fn end_to_end_snippets() {
    test_files();
}
