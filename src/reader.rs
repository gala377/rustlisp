use crate::data::SExpr;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedAtom,
    UnclosedList,
}

enum Token {
    Val(String),
    LeftBracket,
    RightBracket,
    Quote,
}

impl From<&str> for Token {
    fn from(val: &str) -> Self {
        use Token::*;
        match val {
            "(" => LeftBracket,
            ")" => RightBracket,
            "\"" => Quote,
            other => Val(String::from(other)),
        }
    }
}

fn tokenize(source: &str) -> Vec<Token> {
    source
        .replace('(', " ( ")
        .replace(')', " ) ")
        .replace('\"', " \" ")
        .split_whitespace()
        .map(Token::from)
        .collect()
}

pub fn read(source: &str) -> Result<Vec<SExpr>, ParseError> {
    let tokens = tokenize(source);
    parse_program(tokens.into_iter())
}

fn parse_program<It>(mut curr_atom: It) -> Result<Vec<SExpr>, ParseError>
where
    It: Iterator<Item = Token>,
{
    let mut prog = Vec::new();
    while let Some(atom) = curr_atom.next() {
        match atom {
            Token::LeftBracket => prog.push(parse_list(&mut curr_atom)?),
            _ => return Err(ParseError::UnexpectedAtom),
        }
    }
    Ok(prog)
}

fn parse_list<It>(curr_atom: &mut It) -> Result<SExpr, ParseError>
where
    It: Iterator<Item = Token>,
{
    let mut list = Vec::new();
    while let Some(atom) = curr_atom.next() {
        list.push(match atom {
            Token::RightBracket => return Ok(SExpr::List(list)),
            Token::LeftBracket => parse_list(curr_atom)?,
            Token::Quote => parse_string(curr_atom)?,
            Token::Val(val) => parse_atom(val)?,
        });
    }
    Err(ParseError::UnclosedList)
}

fn parse_string<It>(curr_atom: &mut It) -> Result<SExpr, ParseError>
where
    It: Iterator<Item = Token>,
{
    let mut res = String::new();
    while let Some(atom) = curr_atom.next() {
        match atom {
            Token::Val(val) => res.push_str(&val),
            Token::LeftBracket => res.push('('),
            Token::RightBracket => res.push(')'),
            Token::Quote => {
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
