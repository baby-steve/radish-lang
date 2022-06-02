use radish::{self, Module, Namespace, NamespaceBuilder, RadishError, VM};

struct SomePackage;

impl Namespace for SomePackage {
    fn name(&self) -> &str {
        "some_package"
    }

    fn build(&mut self, namespace: &mut NamespaceBuilder) {
        let mut a = Module::new_("a");
        a.add_value("c", 23);

        let mut b = Module::new_("b");
        b.add_value("d", 45);

        namespace.add(a).add(b);
    }
}

fn main() -> Result<(), RadishError> {
    let mut vm = VM::new();

    vm.load_namespace(SomePackage)?;

    let script = r#"
        import "some_package"
        print some_package

        import "some_package/a"
        import "some_package/b"

        print a.c + b.d
    "#;

    vm.exec(script)?;

    Ok(())
}
