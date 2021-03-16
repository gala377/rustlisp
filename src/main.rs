const PROGRAM: &'static str =
    r#"(define (my-function arg1 arg2)
    (my-body-one (+ arg1 arg2 3.123))
    (my-body-two (* arg1 arg2 1 -2)))
    
    (println "Hello people")
    "#;

enum Atom {
    Val(String),
    LeftBracket,
    RightBracket,
    Quote,
}

impl From<&str> for Atom {
    fn from(val: &str) -> Self {
        use Atom::*;
        match val {
            "(" => LeftBracket,
            ")" => RightBracket,
            "\"" => Quote,
            other => Val(String::from(other)),
        }
    }
}


enum SExpr {
    LitNumber(f64),
    LitString(String),
    Symbol(String),
    List(Vec<SExpr>),
}

#[derive(Debug)]
enum ParseError {
    UnexpectedAtom,
    UnclosedList,
}

// Parsing Atom stream

fn parse_program<It>(mut curr_atom: It) -> Result<Vec<SExpr>, ParseError>
where It: Iterator<Item=Atom>
{
    let mut prog = Vec::new();
    while let Some(atom) = curr_atom.next() {
        match atom {
            Atom::LeftBracket => prog.push(parse_list(&mut curr_atom)?),
            _ => return Err(ParseError::UnexpectedAtom),
        }
    }
    Ok(prog)
}

fn parse_list<It>(curr_atom: &mut It) -> Result<SExpr, ParseError>
where It: Iterator<Item=Atom> {
    let mut list = Vec::new();
    while let Some(atom) = curr_atom.next() {
        list.push(match atom {
            Atom::RightBracket => return Ok(SExpr::List(list)),
            Atom::LeftBracket => parse_list(curr_atom)?,
            Atom::Quote => parse_string(curr_atom)?,
            Atom::Val(val) => parse_atom(val)?,
        });
    }
    Err(ParseError::UnclosedList)
}


fn parse_string<It>(curr_atom: &mut It) -> Result<SExpr, ParseError>
where It: Iterator<Item=Atom> {
    let mut res = String::new();
    while let Some(atom) = curr_atom.next() {
        match atom {
            Atom::Val(val) => res.push_str(&val),
            Atom::LeftBracket => res.push('('),
            Atom::RightBracket => res.push(')'),
            Atom::Quote => {
                res.pop();
                break;
            }
        };
        res.push(' ');
    }
    Ok(SExpr::LitString(res))
}

fn parse_atom(curr_atom: String) -> Result<SExpr, ParseError> {
    // `parse` resolves to `f64`.
    // It also handles negative numbers.
    match curr_atom.parse() {
        Ok(val) => Ok(SExpr::LitNumber(val)),
        Err(_) => Ok(SExpr::Symbol(curr_atom)),
    }
}



// Simple printing functions

fn print_ast(ast: &[SExpr]) {
    for val in ast {
        print_sexpr(val);
        print!("\n");
    }
}

fn print_sexpr(expr: &SExpr) {
    print_sexpr_impl(expr, 0);
}

fn print_sexpr_impl(expr: &SExpr, depth: usize) {
    match expr {
        SExpr::LitNumber(val) => print!("{}", val),
        SExpr::LitString(val) => print!(r#""{}""#, val),
        SExpr::Symbol(val) => print!("{}", val),
        SExpr::List(val) => {
            print!("\n{}(", " ".repeat(depth*2));
            val.iter().for_each(|val| {
                print_sexpr_impl(val, depth+1);
                print!(" ");
            });
            print!(")");
        }
    }
}

// main body

fn main() -> Result<(), ParseError> {
    let prog: Vec<Atom> = PROGRAM
        .replace('(', " ( ")
        .replace(')', " ) ")
        .replace('\"', " \" ")
        .split_whitespace()
        .map(Atom::from)
        .collect();
    let ast = parse_program( prog.into_iter())?;
    print_ast(&ast);
    Ok(())
}

