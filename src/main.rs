mod data;
mod eval;
mod reader;
mod stdlib;
mod utils;

use reader::{read, ParseError, AST};
use utils::print_ast;

fn main() -> Result<(), ParseError> {
    let source_code = std::fs::read_to_string("input.rlp").unwrap();
    let AST {
        program,
        mut symbol_table,
    } = read(&source_code)?;
    print!("\n\n\n");
    let env = stdlib::std_env(&mut symbol_table);
    let symbol_table = symbol_table.build();
    print_ast(&program, &symbol_table);
    for expr in &program {
        let val = eval::eval(env.clone(), None, &symbol_table, expr);
        println!("Evaled to value: {}", val.repr(&symbol_table));
    }
    Ok(())
}
