use crate::{
    check_ptr,
    data::{Environment, SymbolTableBuilder},
    eval::{FuncFrame, Interpreter, ModuleState},
    gc::HeapMarked,
    reader::{self, AST},
    runtime::{self, RootedVal},
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
    arg.heap_drop(&mut vm.heap);
    RootedVal::nil(&mut vm.heap)
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
    let symbol_table_builder = SymbolTableBuilder::with_symbols(&mut vm.symbols);
    let AST {
        program,
        mut symbol_table_builder,
    } = reader::load(&source, &mut vm.heap, symbol_table_builder).unwrap();
    let file_env = stdlib::empty_env(&mut symbol_table_builder);
    symbol_table_builder.update_table(&mut vm.symbols);
    vm.call_stack.push(FuncFrame {
        globals: file_env,
        locals: None,
    });
    if load_std_env {
        stdlib::add_std_lib(vm);
    }
    program.iter().for_each(|expr| {
        let res = vm.eval(&expr);
        res.heap_drop(&mut vm.heap);
    });
    runtime::drop_rooted_vec(&mut vm.heap, program);
    vm.call_stack.pop().unwrap().globals
}
