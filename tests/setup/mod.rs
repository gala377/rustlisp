use std::path::Path;

use lispylib::Vm;

pub fn run_rustlisp_file(path: impl AsRef<Path>) {
    let mut vm = Vm::new("testing module");
    let path = Path::new("tests/lispfiles").join(path);
    let source = std::fs::read_to_string(path).unwrap();
    vm.interpret(&source);
    vm.run_gc();
}
