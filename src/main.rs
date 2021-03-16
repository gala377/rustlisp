mod data;
mod reader;
mod utils;

use reader::{read, ParseError};
use utils::print_ast;

const PROGRAM: &'static str = r#"(define (my-function arg1 arg2)
    (my-body-one (+ arg1 arg2 3.123))
    (my-body-two (* arg1 arg2 1 -2)))
    
    (println "Hello people")
    "#;

fn main() -> Result<(), ParseError> {
    let ast = read(PROGRAM)?;
    print_ast(&ast);
    Ok(())
}
