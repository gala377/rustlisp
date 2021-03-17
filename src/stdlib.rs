use std::collections::HashMap;

use crate::data::{Environment, RuntimeVal};

macro_rules! def_func {
    ($map:ident, $name:expr, $lambda:expr) => {
        $map.insert($name.into(), RuntimeVal::native_function($lambda));
    };
    ($map:ident, $name:expr, $func:path) => {
        $map.insert($name.into(), RuntimeVal::native_function($lambda));
    };
}

pub fn std_env() -> Environment {
    let mut env = Environment::new();
    define_prelude_functions(&mut env.borrow_mut().values);
    env
}

fn define_prelude_functions(map: &mut HashMap<String, RuntimeVal>) {
    def_func!(map, "print", |_, args| {
        for arg in args.into_iter() {
            print!("{}", arg.repr());
            print!(" ");
        }
        RuntimeVal::nil()
    });
    def_func!(map, "repr", |_, args| {
        assert!(args.len() == 1);
        RuntimeVal::StringVal(args[0].repr())
    });
}
