use std::collections::HashMap;

use crate::data::{Environment, RuntimeVal};
use crate::utils::concatenate_vectors;

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
        print!("\n");
        RuntimeVal::nil()
    });
    def_func!(map, "repr", |_, args| {
        assert!(args.len() == 1);
        RuntimeVal::StringVal(args[0].repr())
    });
    def_func!(map, "+", plus);
}

fn plus(env: Environment, args: Vec<RuntimeVal>) -> RuntimeVal {
    assert!(args.len() > 1, "Cannot add less than 2 values");
    match &args[0] {
        RuntimeVal::List(_) => concatenate_lists(env, args),
        RuntimeVal::NumberVal(_) => add_numbers(env, args),
        RuntimeVal::StringVal(_) => concatenate_strings(env, args),
        _ => panic!("You can only add lists, numbers or strings"),
    }
}

fn add_numbers(_: Environment, args: Vec<RuntimeVal>) -> RuntimeVal {
    RuntimeVal::NumberVal(args.into_iter().fold(0.0, |acc, x| match x {
        RuntimeVal::NumberVal(inner) => acc + inner,
        _ => panic!("Types mismatched"),
    }))
}

fn concatenate_strings(_: Environment, args: Vec<RuntimeVal>) -> RuntimeVal {
    RuntimeVal::StringVal(args.into_iter().fold(String::new(), |acc, x| match x {
        RuntimeVal::StringVal(inner) => acc + &inner,
        _ => panic!("Types mismatched"),
    }))
}

fn concatenate_lists(_: Environment, args: Vec<RuntimeVal>) -> RuntimeVal {
    RuntimeVal::List(args.into_iter().fold(Vec::new(), |acc, x| match x {
        RuntimeVal::List(inner) => concatenate_vectors(acc, inner),
        _ => panic!("Types mismatched"),
    }))
}
