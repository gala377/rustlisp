use crate::data::{BuiltinSymbols, SExpr, SymbolId, SymbolTableBuilder};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedAtom,
    UnclosedList,
    UnexpectedClosingParenthesis,
    UnexpectedEOF,
}

enum Token {
    Val(String),
    LeftBracket,
    RightBracket,
    Quote,
    SymbolQuote,
    QuasiQuote,
    Unquote,
}

impl From<&str> for Token {
    fn from(val: &str) -> Self {
        use Token::*;
        match val {
            "(" => LeftBracket,
            ")" => RightBracket,
            "\"" => Quote,
            "'" => SymbolQuote,
            "`" => QuasiQuote,
            "," => Unquote,
            other => Val(String::from(other)),
        }
    }
}

fn tokenize(source: &str) -> Vec<Token> {
    source
        .replace('(', " ( ")
        .replace(')', " ) ")
        .replace('\"', " \" ")
        .replace('\'', " ' ")
        .replace('`', " ` ")
        .replace(',', " , ")
        .split_whitespace()
        .map(Token::from)
        .collect()
}

pub struct AST {
    pub program: Vec<SExpr>,
    pub symbol_table: SymbolTableBuilder,
}

pub fn read(source: &str) -> Result<AST, ParseError> {
    let tokens = tokenize(source);
    let mut symbol_table = SymbolTableBuilder::builtin();
    let res = parse_program(tokens.into_iter(), &mut symbol_table)?;
    Ok(AST {
        program: res,
        symbol_table: symbol_table,
    })
}

fn parse_program<It>(
    mut curr_atom: It,
    symbol_table: &mut SymbolTableBuilder,
) -> Result<Vec<SExpr>, ParseError>
where
    It: Iterator<Item = Token>,
{
    let mut prog = Vec::new();
    while let Some(atom) = curr_atom.next() {
        match atom {
            Token::LeftBracket => prog.push(parse_list(&mut curr_atom, symbol_table)?),
            _ => return Err(ParseError::UnexpectedAtom),
        }
    }
    Ok(prog)
}

fn parse_expr<It>(
    curr_atom: &mut It,
    symbol_table: &mut SymbolTableBuilder,
) -> Result<SExpr, ParseError>
where
    It: Iterator<Item = Token>,
{
    if let Some(atom) = curr_atom.next() {
        match atom {
            Token::LeftBracket => parse_list(curr_atom, symbol_table),
            Token::Quote => parse_string(curr_atom),
            Token::Val(val) => parse_atom(val, symbol_table),
            Token::SymbolQuote => parse_quote(curr_atom, symbol_table),
            Token::QuasiQuote => parse_quasiquote(curr_atom, symbol_table),
            Token::Unquote => parse_unquote(curr_atom, symbol_table),
            _ => Err(ParseError::UnexpectedClosingParenthesis),
        }
    } else {
        Err(ParseError::UnexpectedEOF)
    }
}

fn parse_list<It>(
    curr_atom: &mut It,
    symbol_table: &mut SymbolTableBuilder,
) -> Result<SExpr, ParseError>
where
    It: Iterator<Item = Token>,
{
    let mut list = Vec::new();
    while let Some(atom) = curr_atom.next() {
        list.push(match atom {
            Token::RightBracket => return Ok(SExpr::List(list)),
            Token::LeftBracket => parse_list(curr_atom, symbol_table)?,
            Token::Quote => parse_string(curr_atom)?,
            Token::Val(val) => parse_atom(val, symbol_table)?,
            Token::SymbolQuote => parse_quote(curr_atom, symbol_table)?,
            Token::Unquote => parse_unquote(curr_atom, symbol_table)?,
            Token::QuasiQuote => parse_quasiquote(curr_atom, symbol_table)?,
        });
    }
    Err(ParseError::UnclosedList)
}

fn parse_string<It>(
    curr_atom: &mut It,
) -> Result<SExpr, ParseError>
where
    It: Iterator<Item = Token>,
{
    let mut res = String::new();
    while let Some(atom) = curr_atom.next() {
        match atom {
            Token::Val(val) => res.push_str(&val),
            Token::LeftBracket => res.push('('),
            Token::RightBracket => res.push(')'),
            Token::SymbolQuote => res.push('\''),
            Token::QuasiQuote => res.push('`'),
            Token::Unquote => res.push(','),
            Token::Quote => {
                res.pop();
                break;
            }
        };
        res.push(' ');
    }
    Ok(SExpr::LitString(res))
}

fn parse_quote<It>(
    curr_atom: &mut It,
    symbol_table: &mut SymbolTableBuilder,
) -> Result<SExpr, ParseError>
where
    It: Iterator<Item = Token>,
{
    Ok(SExpr::List(vec![
        SExpr::Symbol(BuiltinSymbols::Quote as SymbolId),
        parse_expr(curr_atom, symbol_table)?,
    ]))
}

fn parse_quasiquote<It>(
    curr_atom: &mut It,
    symbol_table: &mut SymbolTableBuilder,
) -> Result<SExpr, ParseError>
where
    It: Iterator<Item = Token>,
{
    Ok(SExpr::List(vec![
        SExpr::Symbol(BuiltinSymbols::Quasiquote as SymbolId),
        parse_expr(curr_atom, symbol_table)?,
    ]))
}

fn parse_unquote<It>(
    curr_atom: &mut It,
    symbol_table: &mut SymbolTableBuilder,
) -> Result<SExpr, ParseError>
where
    It: Iterator<Item = Token>,
{
    Ok(SExpr::List(vec![
        SExpr::Symbol(BuiltinSymbols::Unquote as SymbolId),
        parse_expr(curr_atom, symbol_table)?,
    ]))
}

fn parse_atom(
    curr_atom: String,
    symbol_table: &mut SymbolTableBuilder,
) -> Result<SExpr, ParseError> {
    // `parse` resolves to `f64`.
    // It also handles negative numbers.
    match curr_atom.parse() {
        Ok(val) => Ok(SExpr::LitNumber(val)),
        Err(_) => Ok(SExpr::Symbol(symbol_table.put_symbol(curr_atom))),
    }
}
