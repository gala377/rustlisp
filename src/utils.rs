use crate::data::SExpr;

pub fn print_ast(ast: &[SExpr]) {
    for val in ast {
        print_sexpr(val);
        print!("\n");
    }
}

pub fn print_sexpr(expr: &SExpr) {
    print_sexpr_impl(expr, 0);
}

pub fn print_sexpr_impl(expr: &SExpr, depth: usize) {
    match expr {
        SExpr::LitNumber(val) => print!("{}", val),
        SExpr::LitString(val) => print!(r#""{}""#, val),
        SExpr::Symbol(val) => print!("{}", val),
        SExpr::Quote(val) => {
            print!("'");
            print_sexpr_impl(val, depth)
        }
        SExpr::List(val) => {
            print!("\n{}(", " ".repeat(depth * 2));
            val.iter().for_each(|val| {
                print_sexpr_impl(val, depth + 1);
                print!(" ");
            });
            print!(")");
        }
    }
}

pub fn concatenate_vectors<T>(mut a: Vec<T>, mut b: Vec<T>) -> Vec<T> {
    a.append(&mut b);
    a
}
