use std::{collections::HashMap, mem::ManuallyDrop};

use crate::{
    data::{Environment, SymbolId, SymbolTable, SymbolTableBuilder},
    eval,
    gc::{Heap, MarkSweep, Root},
    reader::{self, AST},
    runtime::{drop_rooted_vec, RootedVal, WeakVal},
    stdlib,
};

macro_rules! def_func {
    ($map:ident, $symbols:ident, $name:literal, $lambda:expr) => {
        let id = $symbols.put_symbol($name.into());
        $map.insert(id, WeakVal::native_function($lambda));
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
    map: &mut HashMap<SymbolId, WeakVal>,
    symbol_table: &mut SymbolTableBuilder,
) {
    use RootedVal::*;
    def_functions!(map, symbol_table, {
        // Generic functions
        "eq?" => |heap, _, _, _, args| {
            let res = match &args[..] {
                [Symbol(a), Symbol(b)] => RootedVal::predicate(a == b),
                [StringVal(a), StringVal(b)] => RootedVal::predicate(unsafe {a.data.get().as_ref()} ==  unsafe { b.data.get().as_ref()}),
                [NumberVal(a), NumberVal(b)] => RootedVal::predicate(a == b),
                [List(a), List(b)] => RootedVal::predicate(a.data == b.data),
                [Lambda(a), Lambda(b)] => RootedVal::predicate(a.data == b.data),
                _ => panic!("Can only compare 2 values of the same type"),
            };
            drop_rooted_vec(heap, args);
            res
        },
        "repr" => |heap, _, _, sym, args| {
            assert!(args.len() == 1);
            let res = RootedVal::string(args[0].repr(sym), heap);
            drop_rooted_vec(heap, args);
            res
        },
        "+" => plus,
        "to-str" => |heap, _, _, sym, args| {
            assert!(args.len() == 1, "function str accepts only one parameter");
            let res =RootedVal::string(args[0].str(sym), heap);
            drop_rooted_vec(heap, args);
            res
        },

        // functions
        "apply" => |heap, gc, env, sym, mut args: Vec<RootedVal>| {
            assert!(!args.is_empty(), "Cannot apply nothing");
            let func = args.remove(0);
            let res = eval::call(heap, gc, env, sym, &func, args);
            func.heap_drop(heap);
            res
        },

        // lists
        "list" => |heap, _, _,  _, args| RootedVal::list_from_rooted(args, heap),
        "null?" => |heap, _, _, _, args| {
            assert_eq!(args.len(), 1, "null takes one argument");
            let res = if let List(inner) = &args[0] {
                RootedVal::predicate(unsafe { inner.data.get().as_ref().is_empty() } )
            } else {
                panic!("null can only be called on lists");
            };
            drop_rooted_vec(heap, args);
            res
        },
        "list?" => |heap, _, _, _, args| {
            assert_eq!(args.len(), 1, "list? takes one argument");
            let res = if let List(_) = &args[0] {
                RootedVal::sym_true()
            } else {
                RootedVal::sym_false()
            };
            drop_rooted_vec(heap, args);
            res
        },
        "map" => |heap, gc, env, sym, mut args| {
            assert_eq!(args.len(), 2, "you have to map function onto a list");
            let (list, func) = (args.pop().unwrap(), args.pop().unwrap());
            let res = if let List(list) = list {
                let res = unsafe { list.data.get().as_ref() }.iter()
                    .map(|x| {
                        let args = vec![x.clone().upgrade(heap)];
                        eval::call(heap, gc, env.clone(), sym, &func, args)
                }).collect();
                heap.drop_root(list);
                RootedVal::list_from_rooted(res, heap)
            } else {
                panic!("you have to map a function onto a list");
            };
            func.heap_drop(heap);
            res
        },

        // io
        "print" => |heap, _, _, sym, args| {
            args.into_iter().for_each(|x| {
                println!("{}", x.str(sym));
                x.heap_drop(heap);
            });
            RootedVal::nil(heap)
        },
        "read-line" => |heap, _, _, _, args| {
            assert!(args.is_empty(), "read-line takes 0 arguments");
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            RootedVal::string(input, heap)
        },
        "load" => load_from_file,
    });
}

fn plus(
    heap: &mut Heap,
    _gc: &mut MarkSweep,
    env: Environment,
    _: &mut SymbolTable,
    args: Vec<RootedVal>,
) -> RootedVal {
    assert!(args.len() > 1, "Cannot add less than 2 values");
    match &args[0] {
        RootedVal::List(_) => concatenate_lists(heap, env, args),
        RootedVal::NumberVal(_) => add_numbers(heap, env, args),
        RootedVal::StringVal(_) => concatenate_strings(heap, env, args),
        _ => panic!("You can only add lists, numbers or strings"),
    }
}

fn load_from_file(
    heap: &mut Heap,
    gc: &mut MarkSweep,
    mut env: Environment,
    symbols: &mut SymbolTable,
    mut args: Vec<RootedVal>,
) -> RootedVal {
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
        RootedVal::List(inner) => match unsafe { inner.data.get().as_ref().as_slice() } {
            [WeakVal::StringVal(_path), WeakVal::Symbol(_as_symbol)] => {
                unimplemented!()
            }
            _ => panic!("this form of load requires a string path and a symbol"),
        },
        RootedVal::StringVal(ref path) => {
            let symbol_table_builder = SymbolTableBuilder::with_symbols(symbols);
            let file_source = unsafe {
                let ptr = path.data.get();
                let path = ptr.as_ref();
                println!("the path we load from is {}", path);
                std::fs::read_to_string(path).unwrap()
            };
            let AST {
                program,
                mut symbol_table_builder,
            } = reader::load(&file_source, symbol_table_builder).unwrap();
            let file_env = stdlib::std_env(&mut symbol_table_builder);
            symbol_table_builder.update_table(symbols);
            // TODO: there are 2 problems here
            // 1st: Functions need to know its global, otherwise they are kinda dynamicly scoped
            // which sucks ass.
            // 2nd: We dont retain information about original global which means that
            // on first gc step we remove all functions and symbols defined in original global env
            // and we have some nice juicy UB everywhere.
            program.into_iter().for_each(|expr| {
                let res = eval::eval(heap, gc, file_env.clone(), None, symbols, &expr);
                res.heap_drop(heap);
            });
            env.update_with(file_env);
        }
        _ => panic!("illegal form of load"),
    }
    arg.heap_drop(heap);
    drop_rooted_vec(heap, args);
    RootedVal::nil(heap)
}

fn add_numbers(_: &mut Heap, _: Environment, args: Vec<RootedVal>) -> RootedVal {
    RootedVal::NumberVal(args.into_iter().fold(0.0, |acc, x| match x {
        RootedVal::NumberVal(inner) => acc + inner,
        _ => panic!("Types mismatched"),
    }))
}

fn concatenate_strings(heap: &mut Heap, _: Environment, args: Vec<RootedVal>) -> RootedVal {
    RootedVal::string(
        args.into_iter().fold(String::new(), |acc, x| match x {
                RootedVal::StringVal(inner) => {
                    let res = acc + unsafe { inner.data.get().as_ref() };
                    heap.drop_root(inner);
                    res
                }
                _ => panic!("Types mismatched"),
        }),
        heap,
    )
}

fn concatenate_lists(heap: &mut Heap, _: Environment, args: Vec<RootedVal>) -> RootedVal {
    let mut init: Root<Vec<WeakVal>> = heap.allocate(Vec::new());
    for list in args.into_iter() {
        match list {
            RootedVal::List(ptr) => {
                heap.mutate_root(&mut init, |init| {
                    init.extend(unsafe { ptr.data.get().as_ref().iter().cloned() });
                });
                heap.drop_root(ptr);
            }
            _ => panic!("Types mismatched"),
        }
    }
    RootedVal::List(init)
}
