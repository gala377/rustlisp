use std::collections::HashMap;

use crate::{
    check_ptr,
    data::{BuiltinSymbols, Environment, SymbolId, SymbolTableBuilder},
    eval::Interpreter,
    gc::HeapMarked,
    runtime::{drop_rooted_vec, RootedVal, WeakVal},
};

mod list;
mod load;

#[macro_export]
macro_rules! native_untyped_fn {
    ($name:ident ( $vm:ident, $args:ident ) $body:expr) => {
        pub fn $name(
            #[allow(unused_variables)] $vm: &mut crate::eval::Interpreter,
            #[allow(unused_mut)] mut $args: Vec<crate::runtime::RootedVal>,
        ) -> crate::runtime::RootedVal {
            $body
        }
    };
}


#[macro_export]
macro_rules! native_typed_fn {
    ($name:ident ( $vm:ident, $( $args:pat ),* ) $body:expr) => {
        pub fn $name(
            #[allow(unused_variable)] $vm: &mut crate::eval::Interpreter,
            #[allow(unused_mut)] mut args: Vec<crate::runtime::RootedVal>,
        ) -> crate::runtime::RootedVal {
            let res = match &mut args[..] {
                [$( $args),*] => {
                    $body
                }
                _ => panic!("{} wrong arguments in call", stringify!($name))
            };
            drop_rooted_vec(&mut $vm.heap, args);
            res
        }
    };
}

#[macro_export]
macro_rules! native_fn {
    (typed $name:ident ( $vm:ident, $( $args:pat ),* ) $body:expr) => {
        crate::native_typed_fn!{ $name ( $vm, $( $args ),* ) $body }
    };
    ($name:ident ( $vm:ident, $args:ident ) $body:expr) => {
        crate::native_untyped_fn!{ $name ( $vm, $args ) $body }
    };
}

#[macro_export]
macro_rules! native_module {
    () => {};
    (typed $name:ident ($vm:ident, $( $args:pat ),* ) $(=>)? $body:expr ; $($rest:tt)* ) => {
        crate::native_typed_fn!{ $name ( $vm, $( $args ),* ) $body }
        crate::native_module!{ $($rest)* }
    };
    ($name:ident ($vm:ident, $args:ident ) $(=>)? $body:expr ; $($rest:tt)*) => {
        crate::native_untyped_fn!{ $name ( $vm, $args ) $body }
        crate::native_module!{ $($rest)* }
    };
}

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

pub fn empty_env(symbol_table: &mut SymbolTableBuilder) -> Environment {
    let mut env = Environment::new();
    define_native_functions(&mut env.borrow_mut().values, symbol_table);
    env
}

pub fn add_std_lib(vm: &mut Interpreter) {
    load::load_from_file(vm, "stdlib/lib.rlp".into(), false);
}

fn define_native_functions(
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
        "-" => minus,
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
        "list" => list::make_list,
        "null?" => list::is_empty,
        "empty?" => list::is_empty,
        "list?" => list::is_list,
        "nth" => list::get_nth_elem,
        "length" => list::get_len,
        "map" => list::map,
        "push!" => list::push,
        "size" => list::size,
        "head" => list::head,
        "tail" => |vm, mut args| {
            assert_eq!(args.len(), 1, "tail takes one argument");
            let list = args.pop().unwrap();
            if let List(list) = list {
                assert!(!vm.heap.deref_ptr(&list).is_empty(), "You cannot take tail from empty list");
                let tail = vm.heap.deref_ptr(&list).iter().skip(1).cloned().collect();
                let res = RootedVal::list(tail, &mut vm.heap);
                vm.heap.drop_root(list);
                res
            } else {
                panic!("you can only use head on a lists");
            }
        },
        // numbers
        "less-than" => less_than,

        // io
        "print" => |vm, args| {
            args.into_iter().for_each(|x| {
                println!("{}", x.str(&mut vm.heap, &mut vm.symbols));
                x.heap_drop(&mut vm.heap);
            });
            RootedVal::none()
        },
        "read-line" => |vm, args| {
            assert!(args.is_empty(), "read-line takes 0 arguments");
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            RootedVal::string(input, &mut vm.heap)
        },
        "load" => load::load_from_file_runtime_wrapper,
        "__load" => load::load_from_file_without_std_env_runtime_wrapper,
        "assert" => assert_impl,
        "print-globals" => print_globals,
    });
}

use RootedVal::*;

native_module!{
    typed less_than(vm, NumberVal(a), NumberVal(b)) => 
        RootedVal::predicate(*a < *b);

    print_globals(vm, args) {
        let globals = vm.get_globals();
        println!("Printing globals:");
        for (k, v) in &globals.borrow().values {
            let printable = v.repr(&vm.heap, &vm.symbols);
            let symbol = vm.symbols[*k].clone();
            println!("\t{:20}|{}", symbol, printable);
        }
        drop_rooted_vec(&mut vm.heap, args);
        RootedVal::none()
    };

    assert_impl(vm, args) {
        let message = args.pop().unwrap();
        let func = vm.get_value("eq?").unwrap();
        let res = vm.call(&func, args);
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
    };

    plus(vm, args) {
        assert!(args.len() > 1, "Cannot add less than 2 values");
        match &args[0] {
            RootedVal::List(_) => concatenate_lists(vm, args),
            RootedVal::NumberVal(_) => add_numbers(vm, args),
            RootedVal::StringVal(_) => concatenate_strings(vm, args),
            _ => panic!("You can only add lists, numbers or strings"),
        }
    };

    typed minus(vm, NumberVal(a), NumberVal(b)) =>
        RootedVal::NumberVal(*a - *b);

    add_numbers(vm, args) {
        RootedVal::NumberVal(args.into_iter().fold(0.0, |acc, x| match x {
            RootedVal::NumberVal(inner) => acc + inner,
            _ => panic!("Types mismatched"),
        }))
    };

    concatenate_strings(vm, args) {
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
    };

    concatenate_lists(vm, args) {
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
    };
}
