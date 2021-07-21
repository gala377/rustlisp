use std::env;

use lispylib::{self, Interpreter, gc::{Heap, MarkSweep}, runtime::drop_rooted_vec};

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
    let mut heap = Heap::with_capacity(1000);
    let lispylib::AST {
        program,
        mut symbol_table_builder,
    } = lispylib::read(&source_code, &mut heap)?;
    print!("\n\n\n");
    let env = lispylib::stdlib::std_env(&mut symbol_table_builder);
    let symbol_table = symbol_table_builder.build();
    let gc = MarkSweep::new();
    let mut interp = Interpreter::new(heap, gc, env, None, symbol_table);
    for expr in &program {
        let res = interp.eval(expr);
        res.heap_drop(&mut interp.heap);
    }
    drop_rooted_vec(&mut interp.heap, program);
    Ok(())
}
