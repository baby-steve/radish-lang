//! Runs snippet tests.

use radish_lang::{
    common::source::Source,
    compiler::{analysis::SemanticAnalyzer, compiler::Compiler, parser::Parser},
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
        //println!("{:?}", expected);

        TestSnippet {
            expected_values: expected,
            source: Rc::clone(&source),
        }
    }

    pub fn run(&self) {
        let result = Parser::new(Rc::clone(&self.source)).parse();

        match result {
            Ok(res) => {
                // Capture all messages printed from the VM.
                let stdout = RadishMockLogger::new();

                // Configuartion with stdout
                let config = RadishConfig::with_stdout(stdout.clone());

                // Analysis
                let mut semantic_analyzer = SemanticAnalyzer::new();
                semantic_analyzer.analyze(&res);

                // Compile
                let mut compiler = Compiler::new();
                compiler.run(&res);

                // Run the VM
                VM::new(&config).interpret(compiler.chunk);

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
                println!("{}", err);
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
