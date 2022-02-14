use assert_cmd::prelude::*;
use predicates::prelude::*;
use std::process::Command;

#[test]
fn file_doesnt_exist() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("radish-lang")?;

    cmd.arg("test/file/doesnt/exist");
    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("No such file or directory"));

    Ok(())
}

#[test]
fn with_arguments() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("radish-lang")?;

    cmd.args(&["tests/snippets/binary_expr.rdsh", "23", "45"]);
    cmd.assert().success();

    Ok(())
}

#[test]
fn with_short_flags() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("radish-lang")?;

    cmd.args(&["-a", "-d", "-t", "tests/snippets/binary_expr.rdsh"]);
    cmd.assert().success();

    Ok(())
}

#[test]
fn with_long_flags() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("radish-lang")?;

    cmd.args(&["--dump-ast", "--dump-bytecode", "--trace", "tests/snippets/binary_expr.rdsh"]);
    cmd.assert().success();

    Ok(())
}