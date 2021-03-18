mod data;
mod eval;
mod reader;
mod stdlib;
mod utils;

use reader::{read, ParseError, AST};

fn main() -> Result<(), ParseError> {
    let source_code = std::fs::read_to_string("input.rlp").unwrap();
    let AST {
        program,
        mut symbol_table,
    } = read(&source_code)?;
    print!("\n\n\n");
    let env = stdlib::std_env(&mut symbol_table);
    let mut symbol_table = symbol_table.build();
    for expr in &program {
        eval::eval(env.clone(), None, &mut symbol_table, expr);
    }
    Ok(())
}
