# The Radish Programming Language
Radish is a small, embeddable scripting language, aiming to be simple, fast and practical.

> **NOTE:**
> Radish is a work in progress and is missing many of its planned language features.

# Examples
## Hello World
```
print "Hello, World!"
```
## FizzBuzz
```
fun fizzbuzz(start, end) {
    var i = start
    while i <= end loop
        if i % 3 == 0 and i % 5 == 0 then
            print "FizzBuzz"
        else if i % 3 == 0 then
            print "Fizz"
        else if i % 5 == 0 then
            print "Buzz" 
        else 
            print i
        endif
        i += 1
    endloop
}

fizzbuzz(0, 100)
```

For additonal examples you can check out the [examples](examples) directory or Radish's [test snippets](tests/snippets).

# Usage

```console
$ cargo run -- --help
radish v0.1.0
The Radish scripting language

USAGE:
    radish-lang [FLAGS] <FILE.rdsh> [arguments]...

FLAGS:
    -a, --dump-ast         Dump the program's AST (Abstract Syntax Tree)
    -d, --dump-bytecode    Dump the program's bytecode
    -h, --help             Prints help information
    -t, --trace            Trace the VM's execution
    -V, --version          Prints version information

ARGS:
    <FILE.rdsh>       Path to file
    <arguments>...    Arguments passed to program
```

# Installation
Right now the only way to use Radish is building it from source and using Cargo to run it. Make sure you have Rust and Cargo [installed](https://www.rust-lang.org/tools/install) before running the following commands:
```console
$ git clone https://github.com/accusatorySteve/radish-lang.git
$ cd radish-lang
$ cargo run -- examples/hello.rdsh
"Hello, World!"
```

# Contributing
All contributions welcome. If you find a bug or have a feature request, feel free to open an issue.
