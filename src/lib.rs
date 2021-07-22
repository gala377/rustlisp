pub mod data;
pub mod eval;
pub mod gc;
pub mod reader;
pub mod runtime;
pub mod stdlib;
pub mod utils;

pub use reader::{read, ParseError, AST};

pub struct Vm {
    interpreter: eval::Interpreter,
}

impl Vm {
    pub fn new(module: &str) -> Self {
        let heap = gc::Heap::with_capacity(10000);
        let mut symbol_table_builder = data::SymbolTableBuilder::builtin();
        let env = stdlib::native_std_env(&mut symbol_table_builder);
        let gc = gc::MarkSweep::new();
        let symbol_table = symbol_table_builder.build();
        let interpreter = eval::Interpreter::new(heap, gc, env, None, symbol_table, module);
        let mut vm = Self { interpreter };
        stdlib::load_non_native_std_env(&mut vm.interpreter);
        vm
    }

    pub fn run_gc(&mut self) {
        self.interpreter.run_gc();
    }

    pub fn get_value<Func, Res>(&mut self, name: &str, func: Func) -> Res
    where
        Func: FnOnce(Option<&runtime::RootedVal>, &gc::Heap) -> Res,
    {
        let val = self.interpreter.get_value(name);
        let res = func(val.as_ref(), &self.interpreter.heap);
        if let Some(inner) = val {
            inner.heap_drop(&mut self.interpreter.heap);
        }
        res
    }

    pub fn interpret(&mut self, source: &str) {
        self.interpret_map(source, |_, _| ())
    }

    pub fn interpret_map<Func, Res>(&mut self, source: &str, func: Func) -> Res
    where
        Func: FnOnce(&runtime::RootedVal, &gc::Heap) -> Res,
    {
        let symbol_table_builder =
            data::SymbolTableBuilder::with_symbols(&mut self.interpreter.symbols);
        let AST {
            program,
            symbol_table_builder,
        } = reader::load(source, &mut self.interpreter.heap, symbol_table_builder).unwrap();
        // add newly read symbols to interpreter.symbols
        symbol_table_builder.update_table(&mut self.interpreter.symbols);
        let nil = runtime::RootedVal::NumberVal(0.0);
        let last_rooted = program.iter().fold(nil, |last, expr| {
            last.heap_drop(&mut self.interpreter.heap);
            self.interpreter.eval(&expr)
        });
        runtime::drop_rooted_vec(&mut self.interpreter.heap, program);
        let result = func(&last_rooted, &self.interpreter.heap);
        last_rooted.heap_drop(&mut self.interpreter.heap);
        self.interpreter.run_gc();
        result
    }
}
