use radish_lang::value::Value;
use radish_lang::vm::VM;
use radish_lang::compiler::Compiler;
use radish_lang::parser::Parser;

fn run_test_vm(src: &str) -> VM {
    let result = Parser::new(src).parse().unwrap();
    let mut compiler = Compiler::new();
    compiler.run(&result);
    let mut vm = VM::new(compiler.chunk);
    vm.interpret();
    vm
}

#[test]
fn test_binary_arithmetic_expr() {
    let test_string = "12 + 7 - 8 / 4 * 5";
    let mut vm = run_test_vm(test_string);

    assert_eq!(vm.stack.peek().unwrap(), Value::Number(9.0));
}

#[test]
fn test_grouping_expr() {
    let test_string = "2 * (3 + 4)";
    let mut vm = run_test_vm(test_string);
    assert_eq!(vm.stack.peek().unwrap(), Value::Number(14.0));

    let test_string = "(3 + 4)";
    let mut vm = run_test_vm(test_string);
    assert_eq!(vm.stack.peek().unwrap(), Value::Number(7.0));

    let test_string = "(5 - 3) * (3 + 4)";
    let mut vm = run_test_vm(test_string);
    assert_eq!(vm.stack.peek().unwrap(), Value::Number(14.0));

    let test_string = "2 * (3 + (4 - 1))";
    let mut vm = run_test_vm(test_string);
    assert_eq!(vm.stack.peek().unwrap(), Value::Number(12.0));

    let test_string = "2 * ((((3 + 4))))";
    let mut vm = run_test_vm(test_string);
    assert_eq!(vm.stack.peek().unwrap(), Value::Number(14.0));

    let test_string = "2 * ((((3))))";
    let mut vm = run_test_vm(test_string);
    assert_eq!(vm.stack.peek().unwrap(), Value::Number(6.0));
}

#[test]
fn test_unary_negate() {
    let test_string = "-2";
    let mut vm = run_test_vm(test_string);
    assert_eq!(vm.chunk.constants[0], Value::Number(2.0));
    assert_eq!(vm.stack.peek().unwrap(), Value::Number(-2.0));

    let test_string = "-2 + 3";
    let mut vm = run_test_vm(test_string);
    assert_eq!(vm.stack.peek().unwrap(), Value::Number(1.0));

    let test_string = "-2 + -10";
    let mut vm = run_test_vm(test_string);
    assert_eq!(vm.stack.peek().unwrap(), Value::Number(-12.0));

    let test_string = "-(2 + 3)";
    let mut vm = run_test_vm(test_string);
    assert_eq!(vm.stack.peek().unwrap(), Value::Number(-5.0));

    let test_string = "-5 - -4";
    let mut vm = run_test_vm(test_string);
    assert_eq!(vm.stack.peek().unwrap(), Value::Number(-1.0));

    let test_string = "-2 * -10";
    let mut vm = run_test_vm(test_string);
    assert_eq!(vm.stack.peek().unwrap(), Value::Number(20.0));

    let test_string = "-----2"; // works, but not recommended.
    let mut vm = run_test_vm(test_string);
    assert_eq!(vm.stack.peek().unwrap(), Value::Number(-2.0));
}
