use crate::{Module, ModuleBuilder, VM};

pub trait Namespace {
    fn build(&mut self, namespace: &mut NamespaceBuilder);
    fn name(&self) -> &str;
}

pub struct NamespaceBuilder {
    modules: Vec<Module>,
}

impl NamespaceBuilder {
    pub fn new() -> Self {
        Self { modules: vec![] }
    }

    pub fn add<M: ModuleBuilder + 'static>(&mut self, module: M) -> &mut Self {
        self.modules
            .push(module.build().expect("failed to build module"));

        self
    }

    pub fn finish(self, vm: &mut VM, name: String) {
        let mut namespace = Module::new(&name);

        for module in self.modules {
            namespace.add_module(module);
        }

        for module in namespace.modules().iter() {
            let name = name.clone() + "/" + &*module.borrow().name;
            vm.loader.load_ref(&name, std::rc::Rc::clone(module));
        }

        vm.load_module(namespace)
            .expect(&format!("failed to load namespace: {}", name));
    }
}
