use std::collections::HashMap;

use crate::{
    data::{Environment, SymbolId, SymbolTable, SymbolTableBuilder},
    eval,
    gc::Heap,
    reader::{self, AST},
    runtime::RuntimeVal,
    stdlib,
    utils::accumulate_vectors,
};

macro_rules! def_func {
    ($map:ident, $symbols:ident, $name:literal, $lambda:expr) => {
        let id = $symbols.put_symbol($name.into());
        $map.insert(id, RuntimeVal::native_function($lambda));
    };
}

macro_rules! def_functions {
    ($map:ident, $symbols:ident, { $($name:literal => $func:expr ),+ $(,)?}) => {
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
        "eq?" => |_, _, _, args| match &args[..] {
            [Symbol(a), Symbol(b)] => RuntimeVal::predicate(a == b),
            [StringVal(a), StringVal(b)] => RuntimeVal::predicate(unsafe {a.data.as_ref() } ==  unsafe { b.data.as_ref()}),
            [NumberVal(a), NumberVal(b)] => RuntimeVal::predicate(a == b),
            [List(a), List(b)] => RuntimeVal::predicate(a.data == b.data),
            [Lambda(a), Lambda(b)] => RuntimeVal::predicate(a.data == b.data),
            _ => panic!("Can only compare 2 values of the same type"),
        },
        "repr" => |heap, _, sym, args| {
            assert!(args.len() == 1);
            RuntimeVal::string(args[0].repr(sym), heap)
        },
        "+" => plus,
        "to-str" => |heap, _, sym, args| {
            assert!(args.len() == 1, "function str accepts only one parameter");
            RuntimeVal::string(args[0].str(sym), heap)
        },

        // functions
        "apply" => |heap, env, sym, mut args: Vec<RuntimeVal>| {
            assert!(!args.is_empty(), "Cannot apply nothing");
            let func = args.remove(0);
            eval::call(heap, env, sym, &func, args)
        },

        // lists
        "list" => |heap, _, _, args| RuntimeVal::list(args, heap),
        "null?" => |_, _, _, args| {
            assert_eq!(args.len(), 1, "null takes one argument");
            if let List(inner) = &args[0] {
                RuntimeVal::predicate(unsafe { inner.data.as_ref().is_empty() } )
            } else {
                panic!("null can only be called on lists");
            }
        },
        "list?" => |_, _, _, args| {
            assert_eq!(args.len(), 1, "list? takes one argument");
            if let List(_) = &args[0] {
                RuntimeVal::sym_true()
            } else {
                RuntimeVal::sym_false()
            }
        },
        "map" => |heap, env, sym, mut args| {
            assert_eq!(args.len(), 2, "you have to map function onto a list");
            let (list, func) = (args.pop().unwrap(), args.pop().unwrap());
            if let List(list) = list {
                let res = unsafe {list.data.as_ref() }.into_iter()
                    .map(|x| eval::call(
                        heap, env.clone(), sym, &func, vec![x.clone()]
                    )).collect();
                RuntimeVal::list(res, heap)
            } else {
                panic!("you have to map a function onto a list");
            }
        },

        // io
        "print" => |heap, _, sym, args| {
            args.into_iter().for_each(|x|{ println!("{}", x.str(sym)) });
            RuntimeVal::nil(heap)
        },
        "read-line" => |heap, _, _, args| {
            assert!(args.is_empty(), "read-line takes 0 arguments");
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            RuntimeVal::string(input, heap)
        },
        "load" => load_from_file,
    });
}

fn plus(
    heap: &mut Heap,
    env: Environment,
    _: &mut SymbolTable,
    args: Vec<RuntimeVal>,
) -> RuntimeVal {
    assert!(args.len() > 1, "Cannot add less than 2 values");
    match &args[0] {
        RuntimeVal::List(_) => concatenate_lists(heap, env, args),
        RuntimeVal::NumberVal(_) => add_numbers(heap, env, args),
        RuntimeVal::StringVal(_) => concatenate_strings(heap, env, args),
        _ => panic!("You can only add lists, numbers or strings"),
    }
}

fn load_from_file(
    heap: &mut Heap,
    mut env: Environment,
    symbols: &mut SymbolTable,
    mut args: Vec<RuntimeVal>,
) -> RuntimeVal {
    //! todo:
    //!
    //! When loading a function from another file if its body
    //! refers to function which exists in both files then the
    //! refereed function changes. We need namespaces.
    //!
    //! file1.rlp
    //! (def val 1)
    //! (def (get-val) val)
    //!
    //! file2.rlp
    //! (def val 5)
    //! (load "file1.rlp")
    //! (eq? (get-val) 5) ; that is true and shouldn't be
    assert_eq!(args.len(), 1, "load takes only one argument");
    let arg = args.pop().unwrap();
    match arg {
        RuntimeVal::List(inner) => match unsafe { inner.data.as_ref().as_slice() } {
            [RuntimeVal::StringVal(_path), RuntimeVal::Symbol(_as_symbol)] => {
                unimplemented!()
            }
            _ => panic!("this form of load requires a string path and a symbol"),
        },
        RuntimeVal::StringVal(ref path) => {
            let symbol_table_builder = SymbolTableBuilder::with_symbols(symbols);
            let path = unsafe { path.data.as_ref() };
            println!("the path we load from is {}", path);
            let file_source = std::fs::read_to_string(path).unwrap();
            let AST {
                program,
                mut symbol_table_builder,
            } = reader::load(&file_source, symbol_table_builder).unwrap();
            let file_env = stdlib::std_env(&mut symbol_table_builder);
            symbol_table_builder.update_table(symbols);
            program.into_iter().for_each(|expr| {
                eval::eval(heap, file_env.clone(), None, symbols, &expr);
            });
            env.update_with(file_env);
        }
        _ => panic!("illegal form of load"),
    }
    RuntimeVal::nil(heap)
}

fn add_numbers(_: &mut Heap, _: Environment, args: Vec<RuntimeVal>) -> RuntimeVal {
    RuntimeVal::NumberVal(args.into_iter().fold(0.0, |acc, x| match x {
        RuntimeVal::NumberVal(inner) => acc + inner,
        _ => panic!("Types mismatched"),
    }))
}

fn concatenate_strings(heap: &mut Heap, _: Environment, args: Vec<RuntimeVal>) -> RuntimeVal {
    RuntimeVal::string(
        args.into_iter().fold(String::new(), |acc, x| match x {
            RuntimeVal::StringVal(inner) => acc + unsafe { inner.data.as_ref() },
            _ => panic!("Types mismatched"),
        }),
        heap,
    )
}

fn concatenate_lists(heap: &mut Heap, _: Environment, args: Vec<RuntimeVal>) -> RuntimeVal {
    RuntimeVal::list(
        args.into_iter().fold(Vec::new(), |acc, x| match x {
            RuntimeVal::List(inner) => accumulate_vectors(acc, unsafe { inner.data.as_ref() }),
            _ => panic!("Types mismatched"),
        }),
        heap,
    )
}
