use std::{collections::HashMap, rc::Rc};

use crate::{
    check_ptr, def_module,
    env::{BuiltinSymbols, Environment, SymbolId, SymbolTable},
    eval::{Interpreter, ModuleState},
    gc::HeapMarked,
    runtime::{RootedVal, WeakVal},
};

mod list;
mod load;
mod native;

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
            #[allow(unused_variables)] $vm: &mut crate::eval::Interpreter,
            #[allow(unused_mut)] mut args: Vec<crate::runtime::RootedVal>,
        ) -> crate::runtime::RootedVal {
            match &mut args[..] {
                [$( $args),*] => {
                    $body
                }
                _ => panic!("{} wrong arguments in call", stringify!($name))
            }
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
macro_rules! native_functions {
    () => {};
    (typed $name:ident ($vm:ident, $( $args:pat ),* ) $(=>)? $body:expr ; $($rest:tt)* ) => {
        crate::native_typed_fn!{ $name ( $vm, $( $args ),* ) $body }
        crate::native_functions!{ $($rest)* }
    };
    ($name:ident ($vm:ident, $args:ident ) $(=>)? $body:expr ; $($rest:tt)*) => {
        crate::native_untyped_fn!{ $name ( $vm, $args ) $body }
        crate::native_functions!{ $($rest)* }
    };
}

macro_rules! def_func {
    ($map:ident, $symbols:ident, $name:literal, $lambda:expr) => {
        let id = $symbols.put_symbol($name);
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

pub fn empty_env(symbol_table: &mut SymbolTable) -> Environment {
    let mut env = Environment::new();
    define_native_functions(&mut env.borrow_mut().values, symbol_table);
    env
}

pub fn add_std_lib(vm: &mut Interpreter) {
    define_native_modules(vm);
    load::load_from_file(vm, "std/lib.rlp".into(), false);
}

fn define_native_modules(vm: &mut Interpreter) {
    def_module!(vm, "std/intrinsic", {
        "print-globals" => print_globals,
        "print-module-globals" => print_module_globals,
        "print-modules" => |vm, _| {
            for module in &vm.modules {
                println!("Module {}", module.0)
            }
            RootedVal::none()
        },
    });

    def_module!(vm, "std/native", {
        "dispatch!" => dispatch,
    });

    def_module!(vm, "std/experimental", {
        "counter" => native::new_counter,
    });

    def_module!(vm, "std/io", {
        "print" => |vm, args| {
            args.into_iter().for_each(|x| {
                println!("{}", x.str(&mut vm.heap, &mut vm.symbols));
            });
            RootedVal::none()
        },
        "read-line" => |vm, args| {
            assert!(args.is_empty(), "read-line takes 0 arguments");
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            RootedVal::string(input, &mut vm.heap)
        },
    });
}

fn define_native_functions(map: &mut HashMap<SymbolId, WeakVal>, symbol_table: &mut SymbolTable) {
    use RootedVal::*;
    def_functions!(map, symbol_table, {
        // Generic functions
        "eq?" => |vm, args| match &args[..] {
                [Symbol(a), Symbol(b)] => RootedVal::predicate(a == b),
                [StringVal(a), StringVal(b)] => {
                    check_ptr!(vm.heap, a);
                    check_ptr!(vm.heap, b);
                    RootedVal::predicate(*vm.heap.deref_ptr(a) == *vm.heap.deref_ptr(b))
                }
                [NumberVal(a), NumberVal(b)] => RootedVal::predicate(a == b),
                [List(a), List(b)] => RootedVal::predicate(a == b),
                [Lambda(a), Lambda(b)] => RootedVal::predicate(a == b),
                [Func(a), Func(b)] => RootedVal::predicate(a == b),
                [NativeFunc(a), NativeFunc(b)] => RootedVal::predicate(Rc::as_ptr(a) == Rc::as_ptr(b)),
                [NativeFunc(_), Func(_)] => RootedVal::sym_false(),
                [Func(_), NativeFunc(_)] => RootedVal::sym_false(),
                [Func(_), Lambda(_)] => RootedVal::sym_false(),
                [Lambda(_), Func(_)] => RootedVal::sym_false(),
                [Lambda(_), NativeFunc(_)] => RootedVal::sym_false(),
                [NativeFunc(_), Lambda(_)] => RootedVal::sym_false(),
                _ => panic!("Can only compare 2 values of the same type"),
        },
        "equal?" => deep_eq,
        "repr" => |vm, args| {
            assert!(args.len() == 1);
            RootedVal::string(args[0].repr(&mut vm.heap, &mut vm.symbols), &mut vm.heap)
        },
        "+" => plus,
        "-" => minus,
        "to-str" => |vm, args| {
            assert!(args.len() == 1, "function str accepts only one parameter");
            RootedVal::string(args[0].str(&mut vm.heap, &mut vm.symbols), &mut vm.heap)
        },
        "not" => not_impl,

        // functions
        "apply" => |vm, mut args: Vec<RootedVal>| {
            assert!(!args.is_empty(), "Cannot apply nothing");
            let func = args.remove(0);
            vm.call(&func, args)
        },

        // lists
        "list" => list::make_list,
        "null?" => list::is_empty,
        "empty?" => list::is_empty,
        "list?" => list::is_list,
        "nth" => list::get_nth_elem,
        "length" => list::get_len,
        "map" => list::map,
        "append" => list::append,
        "copy" => list::copy,
        "push!" => list::push,
        "head" => list::head,
        "tail" => |vm, mut args| {
            assert_eq!(args.len(), 1, "tail takes one argument");
            let list = args.pop().unwrap();
            if let List(list) = list {
                assert!(!vm.heap.deref_ptr(&list).is_empty(), "You cannot take tail from empty list");
                let tail = vm.heap.deref_ptr(&list).iter().skip(1).cloned().collect();
                RootedVal::list(tail, &mut vm.heap)
            } else {
                panic!("you can only use head on a lists");
            }
        },
        // numbers
        "less-than" => less_than,
        "<" => less_than,
        "greater-than" => greater_than,
        ">" => greater_than,

        "load" => load::load_from_file_runtime_wrapper,
        "load-no-std" => load::load_from_file_without_std_env_runtime_wrapper,
        "module-import" => load::import_module_runtime_wrapper,
        "assert" => assert_impl,
        "assert-equal" => assert_equal_impl,
        "assert-true" => assert_true_impl,
        "panic!" => panic_impl,
        "module-lookup-item" => load::module_lookup_item_runtime_wrapper,
        "identity" => identity,

        "box" => box_impl,
        "unbox" => unbox_impl,
    });
}

use RootedVal::*;

native_functions! {
    typed box_impl(vm, arg) =>
        RootedVal::boxed(arg.as_weak(), &mut vm.heap);

    typed unbox_impl(vm, Boxed { inner, .. }) => vm.get_ref(inner).as_root();

    typed identity(vm, arg) => arg.clone();

    typed less_than(vm, NumberVal(a), NumberVal(b)) =>
        RootedVal::predicate(*a < *b);

    typed greater_than(vm, NumberVal(a), NumberVal(b)) =>
        RootedVal::predicate(*a > *b);

    print_globals(vm, _args) {
        let globals = vm.get_globals();
        println!("Printing globals:");
        for (k, v) in &globals.borrow().values {
            let printable = v.repr(&vm.heap, &vm.symbols);
            let symbol = vm.symbols[*k].clone();
            println!("\t{:20}|{}", symbol, printable);
        }
        RootedVal::none()
    };

    typed print_module_globals(vm, StringVal(module)) {
        let mod_path = vm.get_ref(module).clone() + ".rlp";
        println!("Printing globals for module {}", mod_path);
        match vm.modules.get(&mod_path) {
            Some(ModuleState::Evaluating) => println!("Module evaluating"),
            Some(ModuleState::Evaluated(ref globals)) => {
                for (k, v) in &globals.borrow().values {
                    let printable = v.repr(&vm.heap, &vm.symbols);
                    let symbol = vm.symbols[*k].clone();
                    println!("\t{:20}|{}", symbol, printable);
                }
            }
            None => panic!("Unknown module"),
        }
        RootedVal::none()
    };

    assert_impl(vm, args) {
        let message = args.pop().unwrap();
        let func = vm.get_value("eq?").unwrap();
        let eq_msg = format!("{} != {}",
            args[0].repr(&vm.heap, &vm.symbols),
            args[1].repr(&vm.heap, &vm.symbols));
        let res = vm.call(&func, args);
        match res {
            RootedVal::Symbol(x) if x == BuiltinSymbols::True as usize => return RootedVal::sym_true(),
            _ => (),
        };
        match message {
            RootedVal::StringVal(ptr) => {
                check_ptr!(vm.heap, ptr);
                let msg = vm.heap.deref_ptr(&ptr).clone();
                panic!("{} ({})", msg, eq_msg);
            }
            _ => panic!("Assertion failed with unknown message"),
        }
    };

    assert_equal_impl(vm, args) {
        let message = if args.len() == 3 {
            args.pop()
        } else {
            None
        };
        let func = vm.get_value("equal?").unwrap();
        let eq_msg = format!("{} != {}",
            args[0].repr(&vm.heap, &vm.symbols),
            args[1].repr(&vm.heap, &vm.symbols));
        let res = vm.call(&func, args);
        match res {
            RootedVal::Symbol(x) => {
                if x == BuiltinSymbols::True as usize {
                    return RootedVal::sym_true();
                }
            }
            _ => (),
        };
        match message {
            None => {
                panic!("Assertion failed: ({})", eq_msg);
            }
            Some(RootedVal::StringVal(ptr)) => {
                check_ptr!(vm.heap, ptr);
                let msg = vm.heap.deref_ptr(&ptr).clone();
                panic!("{} ({})", msg, eq_msg);
            }
            _ => panic!("Assertion failed with unknown message"),
        }
    };

    typed assert_true_impl(vm, Symbol(s), StringVal(msg)) {
        if *s != BuiltinSymbols::True as usize {
            let eq_msg = format!("{} is not true", &vm.symbols[*s]);
            let msg = vm.heap.deref_ptr(msg).clone();
            panic!("{} ({})", msg, eq_msg);
        }
        RootedVal::none()
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
            args.into_iter().fold(String::new(), |mut acc, x| match x {
                RootedVal::StringVal(inner) => {
                    acc += &vm.heap.deref_ptr(&inner);
                    acc
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
        RootedVal::List(result)
    };

    typed dispatch(vm, UserType(this), StringVal(method), List(method_args)) {
        let method_args = vm.get_ref(method_args).iter()
            .cloned()
            .map(WeakVal::upgrade)
            .collect();
        let method = vm.get_ref(method).clone();
        this.dispatch(vm, &method, method_args)
    };

    typed deep_eq(vm, a, b) {
        RootedVal::predicate(deep_equal_impl(vm, a, b))
    };

    typed not_impl(vm, Symbol(s)) {
        if *s == BuiltinSymbols::True as usize {
            return RootedVal::sym_false();
        }
        if *s == BuiltinSymbols::False as usize {
            return RootedVal::sym_true();
        }
        panic!("Not only works on #t and #f")
    };

    typed panic_impl(vm, val) {
        panic!("{}", val.str(&vm.heap, &vm.symbols))
    };
}

fn deep_equal_impl(vm: &mut Interpreter, a: &RootedVal, b: &RootedVal) -> bool {
    match (a, b) {
        (NumberVal(a), NumberVal(b)) => a == b,
        (StringVal(a), StringVal(b)) => vm.get_ref(a).eq(&*vm.get_ref(b)),
        (Symbol(a), Symbol(b)) => a == b,
        (Func(a), Func(b)) => a == b,
        (NativeFunc(a), NativeFunc(b)) => std::ptr::eq(a.as_ref(), b.as_ref()),
        (Lambda(a), Lambda(b)) => a == b,
        (UserType(a), UserType(b)) => a.data == b.data,
        (List(a), List(b)) => {
            let len_a = vm.get_ref(a).len();
            let len_b = vm.get_ref(b).len();
            if len_a != len_b {
                false
            } else {
                let mut res = true;
                for i in 0..len_a {
                    let (elem_a, elem_b) = (vm.get_ref(a)[i].as_root(), vm.get_ref(b)[i].as_root());
                    res = deep_equal_impl(vm, &elem_a, &elem_b);
                    if !res {
                        break;
                    }
                }
                res
            }
        }
        _ => false,
    }
}
