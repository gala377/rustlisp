use std::collections::HashMap;

use crate::check_ptr;
use crate::data::BuiltinSymbols;
use crate::{
    data::{Environment, SymbolId, SymbolTableBuilder},
    eval::Interpreter,
    gc::HeapMarked,
    runtime::{drop_rooted_vec, RootedVal, WeakVal},
};

mod load;

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

pub fn native_std_env(symbol_table: &mut SymbolTableBuilder) -> Environment {
    let mut env = Environment::new();
    define_native_functions_functions(&mut env.borrow_mut().values, symbol_table);
    env
}

pub fn load_non_native_std_env(vm: &mut Interpreter) {
    load::load_from_file(vm, "stdlib/lib.rlp".into(), false);
}

fn define_native_functions_functions(
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
                    RootedVal::predicate(*vm.heap.deref_ptr(a) == *vm.heap.deref_ptr(b))
                }
                [NumberVal(a), NumberVal(b)] => RootedVal::predicate(a == b),
                [List(a), List(b)] => RootedVal::predicate(a == b),
                [Lambda(a), Lambda(b)] => RootedVal::predicate(a == b),
                _ => panic!("Can only compare 2 values of the same type"),
            };
            drop_rooted_vec(&mut vm.heap, args);
            res
        },
        "repr" => |vm, args| {
            assert!(args.len() == 1);
            let res = RootedVal::string(args[0].repr(&mut vm.heap, &mut vm.symbols), &mut vm.heap);
            drop_rooted_vec(&mut vm.heap, args);
            res
        },
        "+" => plus,
        "to-str" => |vm, args| {
            assert!(args.len() == 1, "function str accepts only one parameter");
            let res = RootedVal::string(args[0].str(&mut vm.heap, &mut vm.symbols), &mut vm.heap);
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
                RootedVal::predicate(vm.heap.deref_ptr(inner).is_empty())
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
                let len = (vm.heap.deref_ptr(&list)).len();
                let mut res = Vec::with_capacity(len);
                for i in 0..len {
                    let item = vm.heap.deref_ptr(&list)[i].clone();
                    let item = item.upgrade(&mut vm.heap);
                    res.push(vm.call(&func, vec![item]));
                }
                vm.heap.drop_root(list);
                RootedVal::list_from_rooted(res, &mut vm.heap)
            } else {
                panic!("you have to map a function onto a list");
            };
            func.heap_drop(&mut vm.heap);
            res
        },
        "head" => |vm, mut args| {
            assert_eq!(args.len(), 1, "head takes one argument");
            let list = args.pop().unwrap();
            if let List(list) = list {
                assert!(!vm.heap.deref_ptr(&list).is_empty(), "You cannot take head from empty list");
                let val = vm.heap.deref_ptr(&list)[0].clone();
                let res = val.upgrade(&mut vm.heap);
                drop_rooted_vec(&mut vm.heap, args);
                res
            } else {
                panic!("you can only use head on a lists");
            }
        },
        "tail" => |vm, mut args| {
            assert_eq!(args.len(), 1, "tail takes one argument");
            let list = args.pop().unwrap();
            if let List(list) = list {
                assert!(!vm.heap.deref_ptr(&list).is_empty(), "You cannot take tail from empty list");
                let tail = vm.heap.deref_ptr(&list).iter().skip(1).cloned().collect();
                let res = RootedVal::list(tail, &mut vm.heap);
                drop_rooted_vec(&mut vm.heap, args);
                res
            } else {
                panic!("you can only use head on a lists");
            }
        },
        // io
        "print" => |vm, args| {
            args.into_iter().for_each(|x| {
                println!("{}", x.str(&mut vm.heap, &mut vm.symbols));
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
        "load" => load::load_from_file_rt_wrapper,
        "__load" => load::load_from_file_without_std_env_rt_wrapper,
        "assert" => assert_impl,
        "print-globals" => print_globals,
    });
}

fn print_globals(vm: &mut Interpreter, args: Vec<RootedVal>) -> RootedVal {
    let globals = vm.get_globals();
    println!("Printing globals:");
    for (k, v) in &globals.borrow().values {
        let printable = v.repr(&vm.heap, &vm.symbols);
        let symbol = vm.symbols[*k].clone();
        println!("\t{:20}|{}", symbol, printable);
    }
    drop_rooted_vec(&mut vm.heap, args);
    RootedVal::nil(&mut vm.heap)
}

fn assert_impl(vm: &mut Interpreter, mut args: Vec<RootedVal>) -> RootedVal {
    // println!("Asserting");
    let message = args.pop().unwrap();
    let func = vm.get_value("eq?").unwrap();
    // println!("Got value for eq? Calling...");
    let res = vm.call(&func, args);
    // println!("After call");
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
            let msg = vm.heap.deref_ptr(&ptr).clone();
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

fn add_numbers(_vm: &mut Interpreter, args: Vec<RootedVal>) -> RootedVal {
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
                let res = acc + &vm.heap.deref_ptr(&inner);
                vm.heap.drop_root(inner);
                res
            }
            _ => panic!("Types mismatched"),
        }),
        &mut vm.heap,
    )
}

fn concatenate_lists(vm: &mut Interpreter, args: Vec<RootedVal>) -> RootedVal {
    let size = args.iter().fold(0, |init, x| {
        init + match x {
            RootedVal::List(ptr) => vm.heap.deref_ptr(ptr).len(),
            _ => panic!("Types mismatched in list concatenation"),
        }
    });
    let mut init: Vec<WeakVal> = Vec::with_capacity(size);
    for list in args.iter() {
        match list {
            RootedVal::List(ptr) => {
                check_ptr!(vm.heap, ptr);
                let list_ref = vm.heap.deref_ptr(ptr);
                init.extend(list_ref.iter().cloned());
            }
            _ => panic!("Types mismatched"),
        }
    }
    let result = vm.heap.allocate(init);
    // Copied values are now rooted after we put result into heap
    // so we can safely drop them.
    drop_rooted_vec(&mut vm.heap, args);
    RootedVal::List(result)
}
