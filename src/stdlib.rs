use std::{collections::HashMap, mem::ManuallyDrop};

use crate::check_ptr;
use crate::data::BuiltinSymbols;
use crate::{
    data::{Environment, SymbolId, SymbolTable, SymbolTableBuilder},
    eval::{self, Interpreter},
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
        "eq?" => |vm, args| {
            let res = match &args[..] {
                [Symbol(a), Symbol(b)] => RootedVal::predicate(a == b),
                [StringVal(a), StringVal(b)] => {
                    check_ptr!(vm.heap, a);
                    check_ptr!(vm.heap, b);
                    RootedVal::predicate(unsafe {a.data.as_ref()} ==  unsafe { b.data.as_ref()})
                }
                [NumberVal(a), NumberVal(b)] => RootedVal::predicate(a == b),
                [List(a), List(b)] => RootedVal::predicate(a.data.as_ptr() == b.data.as_ptr()),
                [Lambda(a), Lambda(b)] => RootedVal::predicate(a.data.as_ptr() == b.data.as_ptr()),
                _ => panic!("Can only compare 2 values of the same type"),
            };
            drop_rooted_vec(&mut vm.heap, args);
            res
        },
        "repr" => |vm, args| {
            assert!(args.len() == 1);
            let res = RootedVal::string(args[0].repr(&mut vm.symbols), &mut vm.heap);
            drop_rooted_vec(&mut vm.heap, args);
            res
        },
        "+" => plus,
        "to-str" => |vm, args| {
            assert!(args.len() == 1, "function str accepts only one parameter");
            let res = RootedVal::string(args[0].str(&mut vm.symbols), &mut vm.heap);
            drop_rooted_vec(&mut vm.heap, args);
            res
        },

        // functions
        "apply" => |vm, mut args: Vec<RootedVal>| {
            assert!(!args.is_empty(), "Cannot apply nothing");
            let func = args.remove(0);
            let res = vm.call(&func, args);
            func.heap_drop(&mut vm.heap);
            res
        },

        // lists
        "list" => |vm, args| RootedVal::list_from_rooted(args, &mut vm.heap),
        "null?" => |vm, args| {
            assert_eq!(args.len(), 1, "null takes one argument");
            let res = if let List(inner) = &args[0] {
                check_ptr!(vm.heap, inner);
                RootedVal::predicate(unsafe { inner.data.as_ref().is_empty() } )
            } else {
                panic!("null can only be called on lists");
            };
            drop_rooted_vec(&mut vm.heap, args);
            res
        },
        "list?" => |vm, args| {
            assert_eq!(args.len(), 1, "list? takes one argument");
            let res = if let List(_) = &args[0] {
                RootedVal::sym_true()
            } else {
                RootedVal::sym_false()
            };
            drop_rooted_vec(&mut vm.heap, args);
            res
        },
        "map" => |vm, mut args| {
            assert_eq!(args.len(), 2, "you have to map function onto a list");
            let (list, func) = (args.pop().unwrap(), args.pop().unwrap());
            let res = if let List(list) = list {
                check_ptr!(vm.heap, list);
                let res = unsafe { list.data.as_ref() }.iter()
                    .map(|x| {
                        let args = vec![x.clone().upgrade(&mut vm.heap)];
                        vm.call(&func, args)
                }).collect();
                vm.heap.drop_root(list);
                RootedVal::list_from_rooted(res, &mut vm.heap)
            } else {
                panic!("you have to map a function onto a list");
            };
            func.heap_drop(&mut vm.heap);
            res
        },

        // io
        "print" => |vm, args| {
            args.into_iter().for_each(|x| {
                println!("{}", x.str(&mut vm.symbols));
                x.heap_drop(&mut vm.heap);
            });
            RootedVal::nil(&mut vm.heap)
        },
        "read-line" => |vm, args| {
            assert!(args.is_empty(), "read-line takes 0 arguments");
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            RootedVal::string(input, &mut vm.heap)
        },
        "load" => load_from_file,
        "assert" => assert_impl,
        "print-globals" => print_globals,
    });
}

fn print_globals(vm: &mut Interpreter, args: Vec<RootedVal>) -> RootedVal {
    let globals = vm.get_globals();
    println!("Printing globals:");
    for (k, v) in &globals.borrow().values {
        let printable = v.repr(&mut vm.symbols);
        let symbol = vm.symbols[*k].clone();
        println!("{}: {}", symbol, printable);
    }
    drop_rooted_vec(&mut vm.heap, args);
    RootedVal::nil(&mut vm.heap)
}

fn assert_impl(vm: &mut Interpreter, mut args: Vec<RootedVal>) -> RootedVal {
    println!("Asserting");
    let message = args.pop().unwrap();
    let func = vm.get_value("eq?").unwrap();
    print!("Got value for eq? Calling...");
    let res = vm.call(&func, args);
    print!("After call");
    match res {
        RootedVal::Symbol(x) => {
            if x == BuiltinSymbols::True as usize {
                message.heap_drop(&mut vm.heap);
                func.heap_drop(&mut vm.heap);
                return RootedVal::sym_true();
            }
        }
        _ => (),
    };
    match message {
        RootedVal::StringVal(ptr) => {
            check_ptr!(vm.heap, ptr);
            let msg = unsafe { ptr.data.as_ref().clone() };
            vm.heap.drop_root(ptr);
            panic!("{}", msg);
        }
        _ => panic!("Assertion failed with unknown message"),
    }
}

fn plus(vm: &mut Interpreter, args: Vec<RootedVal>) -> RootedVal {
    assert!(args.len() > 1, "Cannot add less than 2 values");
    match &args[0] {
        RootedVal::List(_) => concatenate_lists(vm, args),
        RootedVal::NumberVal(_) => add_numbers(vm, args),
        RootedVal::StringVal(_) => concatenate_strings(vm, args),
        _ => panic!("You can only add lists, numbers or strings"),
    }
}

fn load_from_file(vm: &mut Interpreter, mut args: Vec<RootedVal>) -> RootedVal {
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
        RootedVal::List(inner) => match unsafe { inner.data.as_ref().as_slice() } {
            [WeakVal::StringVal(_path), WeakVal::Symbol(_as_symbol)] => {
                unimplemented!()
            }
            _ => panic!("this form of load requires a string path and a symbol"),
        },
        RootedVal::StringVal(ref path) => {
            check_ptr!(vm.heap, path);
            let symbol_table_builder = SymbolTableBuilder::with_symbols(&mut vm.symbols);
            let file_source = unsafe {
                let path = path.data.as_ref();
                println!("the path we load from is {}", path);
                std::fs::read_to_string(path).unwrap()
            };
            let AST {
                program,
                mut symbol_table_builder,
            } = reader::load(&file_source, symbol_table_builder).unwrap();
            let file_env = stdlib::std_env(&mut symbol_table_builder);
            symbol_table_builder.update_table(&mut vm.symbols);
            // this kinda works but not really, we need to have a mapping from
            // file to its globals because otherwise after we end here we have no globals
            // to retain after push

            vm.push_context(file_env, Vec::new());
            program.into_iter().for_each(|expr| {
                let res = vm.eval(&expr);
                res.heap_drop(&mut vm.heap);
            });

            // TODO: ugly hack, pls remove this when we have globals properly handled.
            // we pop laded file globals from the globals stack and add it to the
            // front, so that the last globals are now at the top of the stack
            // but we still retain the globals env so the functions are not
            // garbage collected.
            let ctx = vm.pop_context();
            // todo: we should actually throw on name conflict but we
            // do not have a proper namespacing yet
            vm.get_globals().update_with(ctx.0.clone());
            vm.save_context(ctx);
            // end of hack
        }
        _ => panic!("illegal form of load"),
    }
    arg.heap_drop(&mut vm.heap);
    drop_rooted_vec(&mut vm.heap, args);
    RootedVal::nil(&mut vm.heap)
}

fn add_numbers(vm: &mut Interpreter, args: Vec<RootedVal>) -> RootedVal {
    RootedVal::NumberVal(args.into_iter().fold(0.0, |acc, x| match x {
        RootedVal::NumberVal(inner) => acc + inner,
        _ => panic!("Types mismatched"),
    }))
}

fn concatenate_strings(vm: &mut Interpreter, args: Vec<RootedVal>) -> RootedVal {
    RootedVal::string(
        args.into_iter().fold(String::new(), |acc, x| match x {
            RootedVal::StringVal(inner) => {
                check_ptr!(vm.heap, inner);
                let res = acc + unsafe { inner.data.as_ref() };
                vm.heap.drop_root(inner);
                res
            }
            _ => panic!("Types mismatched"),
        }),
        &mut vm.heap,
    )
}

fn concatenate_lists(vm: &mut Interpreter, args: Vec<RootedVal>) -> RootedVal {
    let mut init: Root<Vec<WeakVal>> = vm.heap.allocate(Vec::new());
    for list in args.into_iter() {
        match list {
            RootedVal::List(ptr) => {
                check_ptr!(vm.heap, ptr);
                vm.heap.mutate_root(&mut init, |init| {
                    init.extend(unsafe { ptr.data.as_ref().iter().cloned() });
                });
                vm.heap.drop_root(ptr);
            }
            _ => panic!("Types mismatched"),
        }
    }
    RootedVal::List(init)
}
