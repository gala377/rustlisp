use std::env;

use lispylib::{
    self,
    gc::{Heap, MarkSweep},
    Interpreter,
};

fn get_file_name(args: Vec<String>) -> String {
    match args.len() {
        1 => "input.rlp".to_string(),
        2 => args[1].clone(),
        _ => panic!("Provide one argument - file to interpret"),
    }
}

fn main() -> Result<(), lispylib::ParseError> {
    let filename = get_file_name(env::args().collect());
    let source_code = std::fs::read_to_string(filename).unwrap();
    let lispylib::AST {
        program,
        mut symbol_table_builder,
    } = lispylib::read(&source_code)?;
    print!("\n\n\n");
    let env = lispylib::stdlib::std_env(&mut symbol_table_builder);
    let symbol_table = symbol_table_builder.build();
    let heap = Heap::with_capacity(1000);
    let gc = MarkSweep::new();
    let mut interp = Interpreter::new(heap, gc, env, None, symbol_table);
    for expr in &program {
        let res = interp.eval(expr);
        res.heap_drop(&mut interp.heap);
    }
    Ok(())
}
