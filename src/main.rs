use std::env;

use lispylib;

fn get_file_name(args: Vec<String>) -> String {
    match args.len() {
        1 => "input.rlp".to_string(),
        2 => args[1].clone(),
        _ => panic!("Provide one argument - file to interpret"),
    }
}

fn main() -> Result<(), lispylib::ParseError> {
    let filename = get_file_name(env::args().collect());
    let source_code = std::fs::read_to_string(&filename).unwrap();
    let mut vm = lispylib::Vm::new(&filename);
    vm.interpret(&source_code);
    Ok(())
}
