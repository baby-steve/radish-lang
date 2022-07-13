use super::{FromValue, Value};
use crate::{vm::trace::Trace, VM};
use std::rc::Rc;

type ReturnTyp = Result<Value, Trace>;
type Fun = dyn Fn(&mut VM, Vec<Value>) -> ReturnTyp;

pub trait RegisterFn<A, R> {
    fn register(self) -> Rc<Fun>;
}

impl<F: Fn() -> ReturnTyp + 'static> RegisterFn<(), ReturnTyp> for F {
    fn register(self) -> Rc<Fun> {
        Rc::new(Box::new(move |_: &mut VM, _: Vec<Value>| (self)()))
    }
}

impl<F: Fn(&mut VM) -> ReturnTyp + 'static> RegisterFn<ReturnTyp, ReturnTyp> for F {
    fn register(self) -> Rc<Fun> {
        Rc::new(Box::new(move |vm: &mut VM, _: Vec<Value>| (self)(vm)))
    }
}

impl<F: Fn(Value) -> ReturnTyp + 'static> RegisterFn<Value, ReturnTyp> for F {
    fn register(self) -> Rc<Fun> {
        Rc::new(Box::new(move |_: &mut VM, mut args: Vec<Value>| {
            let param = get_arg(&mut args)?;

            (self)(param)
        }))
    }
}

impl<F: Fn() -> () + 'static> RegisterFn<(), ()> for F {
    fn register(self) -> Rc<Fun> {
        Rc::new(Box::new(move |_: &mut VM, _: Vec<Value>| {
            (self)();

            Ok(Value::Nil)
        }))
    }
}

macro_rules! register_functions {
    () => {};
    ($param:ident $($others:ident)*) => {
        register_functions!($($others)*);

        impl<
            FN: Fn($param, $($others,)*) -> ReturnTyp + 'static,
            $param: FromValue + 'static,
            $($others: FromValue + 'static,)*
        > RegisterFn<($param, $($others,)*), ReturnTyp> for FN {
            #[allow(non_snake_case)]
            fn register(self) -> Rc<Fun> {
                Rc::new(Box::new(move |_: &mut VM, mut args: Vec<Value>| {
                    let $param = $param::from_value(get_arg(&mut args)?)?;
                    $(let $others = $others::from_value(get_arg(&mut args)?)?;)*

                    // TODO: handle if there's too many arguments.

                    (self)($param, $($others),*)
                }))
            }
        }

        impl<
            FN: Fn(&mut VM, $param, $($others,)*) -> ReturnTyp + 'static,
            $param: FromValue + 'static,
            $($others: FromValue + 'static,)*
        > RegisterFn<($param, $($others,)*), $param> for FN {
            #[allow(non_snake_case)]
            fn register(self) -> Rc<Fun> {
                Rc::new(Box::new(move |vm: &mut VM, mut args: Vec<Value>| {
                    let $param = $param::from_value(get_arg(&mut args)?)?;
                    $(let $others = $others::from_value(get_arg(&mut args)?)?;)*

                    (self)(vm, $param, $($others),*)
                }))
            }
        }
    };
}

register_functions!(A B C D E F G H I J K L M N O P Q R S T U V W X);

fn get_arg(args: &mut Vec<Value>) -> Result<Value, Trace> {
    match args.pop() {
        Some(val) => Ok(val),
        None => Err(Trace::new("Missing argument")),
    }
}
