use radish::{self, Module, Namespace, NamespaceBuilder, RadishError, VM, Value, vm::trace::Trace};

struct SomePackage;

fn test() -> Result<Value, Trace> {
    println!("Hello, World!");

    Ok(Value::Nil)
}

impl Namespace for SomePackage {
    fn name(&self) -> &str {
        "some_package"
    }

    fn build(&mut self, namespace: &mut NamespaceBuilder) {
        let mut a = Module::new("a");
        a.add_value("c", 23);

        let mut b = Module::new("b");
        b.add_value("d", 45);
        b.add_native("test", 0, test);

        namespace.add(a).add(b);
    }
}

fn main() -> Result<(), RadishError> {
    let mut vm = VM::new();

    vm.load_namespace(SomePackage);

    let script = r#"
        import "some_package"
        print some_package

        import "some_package/a"
        import "some_package/b"
        import "some_package/test"

        print a.c + b.d

        test()
    "#;

    vm.exec(script)?;

    Ok(())
}
