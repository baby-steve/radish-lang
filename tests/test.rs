use std::{
    borrow::Cow,
    fs::{self, DirEntry},
    io,
    path::Path,
    process::Command,
};

use lazy_static::lazy_static;
use regex::Regex;

use colored::*;

lazy_static! {
    static ref EXPECTED: Regex = Regex::new("// expect: ?(.*)").unwrap();
    static ref ERROR: Regex = Regex::new("// expect error").unwrap();
    static ref NON_TEST: Regex = Regex::new("// no test").unwrap();
    static ref IGNORE: Regex = Regex::new("// ignore").unwrap();
}

enum CreateTestResult {
    Ignore,
    NoTest,
    Some(Test),
}

#[derive(Default)]
struct TestRunner {
    pub passed: u32,
    pub failed: u32,
    pub ignored: u32,
    pub failures: Vec<Test>,
    // pub skipped: u32,
    // pub expectations: u32,
}

#[derive(Debug)]
struct Test {
    path: String,
    output: Vec<(String, u32)>,
    _errors: Vec<u32>,
    failures: Vec<String>,
}

impl Test {
    pub fn new(path: impl ToString) -> CreateTestResult {
        let mut output = vec![];
        let mut _errors = vec![];

        let mut line_num = 0u32;

        let path = path.to_string();

        let contents = match fs::read_to_string(&path) {
            Ok(c) => c,
            Err(err) => panic!("failed to read file: {}", err),
        };

        for line in contents.lines() {
            line_num += 1;

            if line.is_empty() {
                continue;
            }

            if let Some(caps) = EXPECTED.captures(line) {
                output.push((caps[1].to_string(), line_num));
            }

            if ERROR.is_match(line) {
                _errors.push(line_num);
            }

            if NON_TEST.is_match(line) {
                return CreateTestResult::NoTest;
            }

            if IGNORE.is_match(line) {
                return CreateTestResult::Ignore;
            }
        }

        let test = Self {
            path,
            output,
            _errors,
            failures: vec![],
        };

        CreateTestResult::Some(test)
    }

    pub fn run(&mut self) {
        let process = Command::new("target/debug/cli").arg(&self.path)
                .output()
                .expect("failed to start command");

        let out = String::from_utf8_lossy(&process.stdout);
        let err = String::from_utf8_lossy(&process.stderr);

        self.validate(out, err);
    }

    fn validate(&mut self, out: Cow<'_, str>, err: Cow<'_, str>) {
        // TODO: validate expected errors.

        self.validate_output(out, err);
    }

    fn validate_output(&mut self, out: Cow<'_, str>, err: Cow<'_, str>) {
        let mut lines: Vec<&str> = out.split('\n').collect();

        lines.pop(); // remove trailing newline.

        let mut index = 0;

        for line in lines {
            if index >= self.output.len() {
                self.fail(format!("Got output \"{}\" but expected nothing.", line));
            } else if &self.output[index].0 != line {
                self.fail(format!(
                    "Expected \"{}\" on line {} but got \"{}\".",
                    self.output[index].0, self.output[index].1, line
                ));
            }

            index += 1;
        }

        while index < self.output.len() {
            self.fail(format!(
                "Missing expected output \"{}\" on line \"{}\".",
                self.output[index].0, self.output[index].1
            ));

            index += 1;
        }

        if !self.failures.is_empty() && !err.is_empty() {
            self.fail(format!("stderr: {}", err));
        }
    }

    fn fail(&mut self, msg: impl ToString) {
        self.failures.push(msg.to_string());
    }
}

fn visit_dirs(
    dir: &Path,
    callback: &dyn Fn(&DirEntry, &mut TestRunner),
    test_runner: &mut TestRunner,
) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();

            if path.is_dir() {
                visit_dirs(&path, callback, test_runner)?;
            } else {
                callback(&entry, test_runner);
            }
        }
    }
    Ok(())
}

fn run_script(file: &DirEntry, test_runner: &mut TestRunner) {
    let path = file.path();

    if path.extension().is_none() || path.extension().unwrap() != "rdsh" {
        return;
    }

    let path = path.to_string_lossy();

    
    // TODO: allow running only a single set of tests.
    
    let mut test = match Test::new(path.to_string()) {
        CreateTestResult::Ignore => {
            test_runner.ignored += 1;
            return
        }
        CreateTestResult::NoTest => return,
        CreateTestResult::Some(test) => test,
    };
    
    print!("test {} ... ", path);
    
    test.run();

    if test.failures.is_empty() {
        test_runner.passed += 1;

        println!("{}", "ok".green());
    } else {
        test_runner.failed += 1;

        println!("{}", "FAILED".red());

        test_runner.failures.push(test);
    }
}

fn main() {
    let mut test_runner = TestRunner::default();

    println!();

    visit_dirs(
        &Path::new("tests/language"),
        &run_script,
        &mut test_runner,
    )
    .unwrap();

    println!();

    if test_runner.failed == 0 {
        let w = if test_runner.ignored == 1 { "was" } else { "where" };

        println!(
            "All {} tests passed. Yeah! ({} {} ignored)",
            test_runner.passed.to_string().green(),
            test_runner.ignored.to_string().yellow(),
            w,
        );
        println!("Of course there may be a bug in the test suite itself...");
    } else {
        for test in test_runner.failures.iter() {
            println!("---- {} ----", test.path);

            for failure in test.failures.iter() {
                println!("    {}", failure);
            }

            println!();
        }

        let end1 = if test_runner.passed == 1 { "" } else { "s" };
        let end2 = if test_runner.failed == 1 { "" } else { "s" };
        let end3 = if test_runner.ignored == 1 { "" } else { "s" };

        println!(
            "{} test{} passed. {} test{} failed. {} test{} ignored",
            test_runner.passed.to_string().green(),
            end1,
            test_runner.failed.to_string().red(),
            end2,
            test_runner.ignored.to_string().yellow(),
            end3,
        );
    }
}
