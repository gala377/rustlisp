use crate::{check_ptr, env::{Environment, SymbolId}, eval::{FuncFrame, Interpreter, ModuleState}, gc::HeapMarked, native_functions, reader::{self, AST}, runtime::RootedVal, stdlib};

pub fn load_from_file_without_std_env_runtime_wrapper(
    vm: &mut Interpreter,
    args: Vec<RootedVal>,
) -> RootedVal {
    _load_from_file_runtime_wrapper(vm, args, false)
}

pub fn load_from_file_runtime_wrapper(vm: &mut Interpreter, args: Vec<RootedVal>) -> RootedVal {
    _load_from_file_runtime_wrapper(vm, args, true)
}

fn _load_from_file_runtime_wrapper(
    vm: &mut Interpreter,
    mut args: Vec<RootedVal>,
    load_std_env: bool,
) -> RootedVal {
    assert_eq!(args.len(), 1, "load takes only one argument");
    let arg = args.pop().unwrap();
    match arg {
        RootedVal::StringVal(ref path) => {
            check_ptr!(vm.heap, path);
            let file_path = {
                let path = vm.heap.deref_ptr(path);
                println!("the path we load from is {}", *path);
                path.clone()
            };
            load_from_file(vm, file_path, load_std_env);
        }
        _ => panic!("illegal form of load"),
    }
    RootedVal::none()
}

pub fn load_from_file(vm: &mut Interpreter, file_path: String, load_std_env: bool) {
    let module_globals = match vm.modules.get(&file_path) {
        Some(ModuleState::Evaluated(module)) => module.clone(),
        Some(ModuleState::Evaluating) => panic!("Circular dependency {}", file_path),
        None => {
            vm.modules
                .insert(file_path.clone(), ModuleState::Evaluating);
            let source =
                std::fs::read_to_string(&file_path).expect(&format!("No such path {}", file_path));
            let module = load_module(vm, &source, load_std_env);
            let module_entry = vm.modules.get_mut(&file_path).unwrap();
            *module_entry = ModuleState::Evaluated(module.clone());
            module
        }
    };
    vm.get_globals().update_with(module_globals);
}

fn load_module(vm: &mut Interpreter, source: &str, load_std_env: bool) -> Environment {
    let symbol_table = &mut vm.symbols;
    let AST { program } = reader::read(&source, &mut vm.heap, symbol_table).unwrap();
    let file_env = stdlib::empty_env(symbol_table);
    vm.call_stack.push(FuncFrame {
        globals: file_env,
        locals: None,
    });
    if load_std_env {
        stdlib::add_std_lib(vm);
    }
    program.into_iter().for_each(|expr| {
        vm.eval(&expr);
    });
    vm.call_stack.pop().unwrap().globals
}

fn module_lookup_item(vm: &mut Interpreter, module: &str, item: SymbolId) -> RootedVal {
    match vm.modules.get(module) {
        None => panic!("Not module {} found", module),
        Some(ModuleState::Evaluating) => panic!("Cannot load from unevaluated module {}", module),
        Some(ModuleState::Evaluated(globals)) => {
            match globals.borrow().values.get(&item) {
                None => panic!("No item {} in module {}", vm.symbols[item], module),
                Some(item) => item.as_root(),
            }
        }
    }
}

use crate::runtime::RootedVal::*;

native_functions! {
    typed module_lookup_item_runtime_wrapper(vm, Symbol(module), Symbol(item)) {
        let module = vm.symbols[*module].clone() + ".rlp";
        module_lookup_item(vm, &module, *item)
    };
}
