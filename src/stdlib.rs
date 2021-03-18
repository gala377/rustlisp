use std::collections::HashMap;

use crate::data::{Environment, RuntimeVal, SymbolId, SymbolTable, SymbolTableBuilder};
use crate::utils::concatenate_vectors;

macro_rules! def_func {
    ($map:ident, $symbols:ident, $name:expr, $lambda:expr) => {
        let id = $symbols.put_symbol($name.into());
        $map.insert(id, RuntimeVal::native_function($lambda));
    };
    ($map:ident, $symbols:ident, $name:expr, $func:path) => {
        let id = $symbols.put_symbol($name.into());
        $map.insert(id, RuntimeVal::native_function($lambda));
    };
}

pub fn std_env(symbol_table: &mut SymbolTableBuilder) -> Environment {
    let mut env = Environment::new();
    define_prelude_functions(&mut env.borrow_mut().values, symbol_table);
    env
}

fn define_prelude_functions(
    map: &mut HashMap<SymbolId, RuntimeVal>,
    symbol_table: &mut SymbolTableBuilder,
) {
    def_func!(map, symbol_table, "print", |_, sym, args| {
        for arg in args.into_iter() {
            print!("{}", arg.repr(sym));
            print!(" ");
        }
        print!("\n");
        RuntimeVal::nil()
    });
    def_func!(map, symbol_table, "repr", |_, sym, args| {
        assert!(args.len() == 1);
        RuntimeVal::StringVal(args[0].repr(sym))
    });
    def_func!(map, symbol_table, "+", plus);
    def_func!(map, symbol_table, "list", |_, _, args| RuntimeVal::List(
        args
    ));
    def_func!(map, symbol_table, "str", |_, sym, args| {
        assert!(args.len() == 1, "function str accepts only one parameter");
        RuntimeVal::StringVal(args[0].str(sym))
    });
}

fn plus(env: Environment, _: &SymbolTable, args: Vec<RuntimeVal>) -> RuntimeVal {
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
