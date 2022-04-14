//! Runs snippet tests.
use radish::{common::source::Source, VM};

use std::{fs, path::PathBuf, rc::Rc};

use lazy_static::lazy_static;
use regex::Regex;

// A logger to capture the VM's output.
// struct RadishMockLogger {
//     buffer: RefCell<Vec<String>>,
// }
//
// impl RadishMockLogger {
//     pub fn new() -> Rc<RadishMockLogger> {
//         Rc::new(RadishMockLogger {
//             buffer: RefCell::new(vec![]),
//         })
//     }
// }
//
// impl RadishFile for RadishMockLogger {
//     fn write(&self, msg: &str) {
//         self.buffer.borrow_mut().push(String::from(msg));
//     }
// }

struct TestSnippet {
    _expected_values: Vec<String>,
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
            _expected_values: expected,
            source: Rc::clone(&source),
        }
    }

    pub fn run(&self) {
        println!("testing {:?}...", &self.source.path);

        //let stdout = RadishMockLogger::new();

        let mut vm = VM::new();

        if let Err(err) = vm.exec(&self.source.contents) {
            err.emit();
            panic!("test failed");
        }

        // for (_i, _value) in stdout.buffer.borrow().iter().enumerate() {
        // todo!();

        //let expected_value = &self.expected_values[i];
        //if expected_value != value {
        //    println!("expected '{}', but got '{}'", expected_value, value);
        //    panic!("test failed");
        //}
        // }
    }
}

fn test_files() {
    let paths = fs::read_dir("./snippets").expect("You must be in base directory to run tests");

    let mut files: Vec<PathBuf> = vec![];

    for path in paths {
        files.push(path.expect("Could not read path").path());
    }

    println!("Running {:?} test file(s)...\n", &files.len());

    while let Some(path) = files.pop() {
        let file_contents = fs::read_to_string(&path).expect("Could not read file");
        let source = Source::new(&file_contents, &path.to_string_lossy());

        TestSnippet::new(source).run();
    }
}

#[test]
fn end_to_end_snippets() {
    test_files();
}
