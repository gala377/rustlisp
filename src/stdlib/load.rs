use crate::{
    env::{Environment, SymbolId, SymbolTable},
    eval::{FuncFrame, Interpreter, ModuleState},
    native_functions,
    reader::{self, AST},
    runtime::{RootedVal, WeakVal},
    stdlib,
};

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
            let file_path = vm.get_ref(path).clone() + ".rlp";
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
        Some(ModuleState::Evaluated(globals)) => match globals.borrow().values.get(&item) {
            None => panic!("No item {} in module {}", vm.symbols[item], module),
            Some(item) => item.as_root(),
        },
    }
}

#[derive(Debug)]
struct ImportSymbol {
    pub symbol: SymbolId,
    pub rename: Option<SymbolId>,
}

fn import_module(vm: &mut Interpreter, module_path: &str, imports: &[ImportSymbol]) {
    let mut module = match vm.modules.get(module_path) {
        Some(ModuleState::Evaluated(module)) => module.clone(),
        Some(ModuleState::Evaluating) => panic!("Circular dependency {}", module_path),
        None => {
            vm.modules
                .insert(module_path.to_owned(), ModuleState::Evaluating);
            let source = std::fs::read_to_string(module_path)
                .expect(&format!("No such path {}", module_path));
            let module = load_module(vm, &source, true);
            let module_entry = vm.modules.get_mut(module_path).unwrap();
            *module_entry = ModuleState::Evaluated(module.clone());
            module
        }
    };
    for import in imports {
        let val = match module.borrow().values.get(&import.symbol) {
            Some(val) => val.clone(),
            None => panic!(
                "Module {} does not export symbol {}",
                module_path, vm.symbols[import.symbol]
            ),
        };
        let name = match import.rename {
            None => import.symbol,
            Some(name) => name,
        };
        vm.get_globals().borrow_mut().values.insert(name, val);
    }
}

use crate::runtime::RootedVal::*;

native_functions! {
    typed module_lookup_item_runtime_wrapper(vm, StringVal(module), Symbol(item)) {
        let module = vm.get_ref(module).clone() + ".rlp";
        module_lookup_item(vm, &module, *item)
    };

    typed import_module_runtime_wrapper(vm, StringVal(module), exports @ ..) {
        let path = vm.get_ref(module).clone() + ".rlp";
        let exports: Vec<_> = exports.iter().map(|val| match val {
            List(inner) if vm.get_ref(inner).len() == 2 => {
                let inner_ref = vm.get_ref(inner);
                if let [WeakVal::Symbol(name), WeakVal::Symbol(rename)] = &inner_ref[..] {
                    ImportSymbol { symbol: *name, rename: Some(*rename) }
                } else {
                    panic!("Imports should be symbols or lists of 2 elements - symbol to import and rename");
                }
            }
            Symbol(inner) => ImportSymbol{ symbol: *inner, rename: None },
            _ => panic!("Imports should be symbols or lists of 2 elements - symbol to import and rename"),
        }).collect();
        import_module(vm, &path, &exports);
        RootedVal::none()
    };
}
