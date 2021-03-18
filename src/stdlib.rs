use std::collections::HashMap;

use crate::data::{Environment, RuntimeVal, SymbolId, SymbolTable, SymbolTableBuilder};
use crate::eval;
use crate::utils::concatenate_vectors;

macro_rules! def_func {
    ($map:ident, $symbols:ident, $name:literal, $lambda:expr) => {
        let id = $symbols.put_symbol($name.into());
        $map.insert(id, RuntimeVal::native_function($lambda));
    };
}

macro_rules! def_functions {
    ($map:ident, $symbols:ident, { $($name:literal => $func:expr ),+}) => {
        $(
            def_func!($map, $symbols, $name, $func);
         )+
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
    use RuntimeVal::*;
    def_functions!(map, symbol_table, {
        // Generic functions
        "eq?" => |_, _, args| match &args[..] {
            [Symbol(a), Symbol(b)] => RuntimeVal::predicate(a == b),
            [StringVal(a), StringVal(b)] => RuntimeVal::predicate(a == b),
            [NumberVal(a), NumberVal(b)] => RuntimeVal::predicate(a == b),
            _ => panic!("Can only compare 2 values of the same type"),
        },
        "repr" => |_, sym, args| {
            assert!(args.len() == 1);
            StringVal(args[0].repr(sym))
        },
        "plus" => plus,
        "to-str" => |_, sym, args| {
            assert!(args.len() == 1, "function str accepts only one parameter");
            StringVal(args[0].str(sym))
        },

        // functions
        "apply" => |env, sym, mut args: Vec<RuntimeVal>| {
            assert!(!args.is_empty(), "Cannot apply nothing");
            let func = args.remove(0);
            eval::call(env, sym, &func, args)
        },

        // lists
        "list" => |_, _, args| List(args),
        "null" => |_, _, args| {
            assert_eq!(args.len(), 1, "null takes one argument");
            if let List(inner) = &args[0] {
                RuntimeVal::predicate(inner.is_empty())
            } else {
                panic!("null can only be called on lists");
            }
        },
        "list?" => |_, _, args| {
            assert_eq!(args.len(), 1, "list? takes one argument");
            if let List(_) = &args[0] {
                RuntimeVal::sym_true()
            } else {
                RuntimeVal::sym_false()
            }
        },
        "map" => |env, sym, mut args| {
            assert_eq!(args.len(), 2, "you have to map function onto a list");
            let (list, func) = (args.pop().unwrap(), args.pop().unwrap());
            if let List(list) = list {
                let res = list.into_iter()
                    .map(|x| eval::call(
                        env.clone(), sym, &func, vec![x]
                    )).collect();
                List(res)
            } else {
                panic!("you have to map a function onto a list");
            }
        },

        // io
        "print" => |_, sym, args| {
            args.into_iter().for_each(|x|{ println!("{}", x.str(sym)) });
            RuntimeVal::nil()
        },
        "read-line" => |_, _, args| {
            assert!(args.is_empty(), "read-line takes 0 arguments");
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            StringVal(input)
        }
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
