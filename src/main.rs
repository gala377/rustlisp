use std::env;

use lispylib::{self, gc::Heap};

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
    let mut symbol_table = symbol_table_builder.build();
    let mut heap = Heap::with_capacity(1000);
    for expr in &program {
        let res = lispylib::eval(&mut heap, env.clone(), None, &mut symbol_table, expr);
        res.heap_drop(&mut heap);
    }
    Ok(())
}
