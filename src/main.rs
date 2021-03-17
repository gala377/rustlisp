mod data;
mod eval;
mod reader;
mod stdlib;
mod utils;


use reader::{read, ParseError};
use utils::print_ast;

const PROGRAM: &'static str = r#"
    (repr "hello world")
    (def (name) "Rafał")
    (def (greeting name) (+ "Hello:" name))
    (repr "defined Rafał")
    (print "hello" (name) "new line pls")
    (repr "printed name")
    (def Ania "Ania")
    (def (id x) x)
    (print (id Ania))
    (print (greeting "Dupa"))
"#;

fn main() -> Result<(), ParseError> {
    let ast = read(PROGRAM)?;
    print_ast(&ast);
    print!("\n\n\n");
    let env = stdlib::std_env();
    for expr in &ast {
        let val = eval::eval(env.clone(), None, expr);
        println!("Evaled to value: {}", val.repr());
    }
    Ok(())
}
