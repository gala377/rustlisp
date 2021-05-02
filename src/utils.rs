use crate::data::{SExpr, SymbolTable};

pub fn print_ast(ast: &[SExpr], symbols: &SymbolTable) {
    for val in ast {
        print_sexpr(val, symbols);
        print!("\n");
    }
}

pub fn print_sexpr(expr: &SExpr, symbols: &SymbolTable) {
    print_sexpr_impl(expr, symbols, 0);
}

pub fn print_sexpr_impl(expr: &SExpr, symbols: &SymbolTable, depth: usize) {
    match expr {
        SExpr::LitNumber(val) => print!("{}", val),
        SExpr::LitString(val) => print!(r#""{}""#, val),
        SExpr::Symbol(val) => print!("{}", symbols[*val]),
        SExpr::List(val) => {
            print!("\n{}(", " ".repeat(depth * 2));
            val.iter().for_each(|val| {
                print_sexpr_impl(val, symbols, depth + 1);
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
