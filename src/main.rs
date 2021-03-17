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
    (print '(greeting (id 1) (id x) (smth smth) 1 2 3))
    
    (repr "hej")
    (def (times x) (+ x x))
    (def (apply func arg) (func arg))
    (apply times 10)
    (def _ (begin 
        (print 1)
        (print 2)
        (print 3)
        1
        "hello"
        "thats the result"
    ))
    (def (myfunc)
        (print "begin")
        10
        (print "end")
        "function ended")
    (print _)
    (myfunc)
    (quote ,(`myfunc print (my smth) 1))
    (print `(,(id 1) ,(id 2) (id (id 3))))
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
