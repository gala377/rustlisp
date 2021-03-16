mod data;
mod reader;
mod utils;
mod stdlib;
mod eval;

use reader::{read, ParseError};
use utils::print_ast;

// const PROGRAM: &'static str = r#"(define (my-function arg1 arg2)
//     (my-body-one (+ arg1 arg2 3.123))
//     (my-body-two (* arg1 arg2 1 -2)))
    
//     (println "Hello people")
//     "#;

const PROGRAM: &'static str = r#"
    (print "hello world")
    (repr -1.2)
    (repr (print 3 4 5 "dupa"))
"#;

fn main() -> Result<(), ParseError> {
    let ast = read(PROGRAM)?;
    print_ast(&ast);
    print!("\n\n\n");
    let env = stdlib::std_env();
    for expr in &ast {
        let val = eval::eval(env.clone(), expr);
        println!("Evaled to value: {}", val.repr());
    }
    Ok(())
}
