use super::Value;

use super::to_value::ToValue;

pub trait ToArgs {
    fn to_args(self) -> Vec<Value>;
}

impl<T> ToArgs for T
where
    T: ToValue,
{
    fn to_args(self) -> Vec<Value> {
        vec![self.to_value()]
    }
}

macro_rules! tuple_to_args {
    ($($name:ident)*) => {
        impl<$($name,)*> ToArgs for ($($name,)*) where $($name: ToValue,)* {
            #[allow(non_snake_case)]
            fn to_args(self) -> Vec<Value> {
                let ($($name,)*) = self;

                vec![$($name.to_value(),)*]
            }
        }
    };
}

tuple_to_args!(A);
tuple_to_args!(A B);
tuple_to_args!(A B C);
tuple_to_args!(A B C D);
tuple_to_args!(A B C D E);
tuple_to_args!(A B C D E F);
tuple_to_args!(A B C D E F G);
tuple_to_args!(A B C D E F G H);
tuple_to_args!(A B C D E F G H I);
tuple_to_args!(A B C D E F G H I J);
tuple_to_args!(A B C D E F G H I J K);
tuple_to_args!(A B C D E F G H I J K L);
tuple_to_args!(A B C D E F G H I J K L M);
tuple_to_args!(A B C D E F G H I J K L M N);
tuple_to_args!(A B C D E F G H I J K L M N O);
tuple_to_args!(A B C D E F G H I J K L M N O P);
tuple_to_args!(A B C D E F G H I J K L M N O P Q);
tuple_to_args!(A B C D E F G H I J K L M N O P Q R);
tuple_to_args!(A B C D E F G H I J K L M N O P Q R S);
tuple_to_args!(A B C D E F G H I J K L M N O P Q R S T);
tuple_to_args!(A B C D E F G H I J K L M N O P Q R S T U);
tuple_to_args!(A B C D E F G H I J K L M N O P Q R S T U V);
tuple_to_args!(A B C D E F G H I J K L M N O P Q R S T U V W);
tuple_to_args!(A B C D E F G H I J K L M N O P Q R S T U V W X);

#[cfg(test)]
mod tests {
    use super::*;

    fn test<A: ToArgs>(args: A) -> Vec<Value> {
        args.to_args()
    }

    #[test]
    fn test_to_args() {
        assert_eq!(test(Value::Nil), vec![Value::Nil]);
        assert_eq!(test(23), vec![Value::Number(23.0)]);
        assert_eq!(
            test((23, (), 89.0)),
            vec![Value::Number(23.0), Value::Nil, Value::Number(89.0)]
        );
    }
}
