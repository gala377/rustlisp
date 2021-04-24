use crate::data::{BuiltinSymbols, SExpr, SymbolId, SymbolTableBuilder};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedAtom,
    UnclosedList,
    UnexpectedClosingParenthesis,
    UnexpectedEOF,
}

#[derive(Debug)]
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

/// Abstract representation of an unevaluated program as a tree structure.
/// Program is represented as a sequence of s-expressions to evaluate in sequence.
/// Additional symbol table is provided with all the symbols that were found
/// during parsing and all symbols denoting special forms.
pub struct AST {
    pub program: Vec<SExpr>,
    pub symbol_table_builder: SymbolTableBuilder,
}

/// Parses source code passed as a string.
/// Successful parsing returns unevaluated AST struct.
pub fn read(source: &str) -> Result<AST, ParseError> {
    let tokens = tokenize(source);
    let mut symbol_table_builder = SymbolTableBuilder::builtin();
    let program = parse_program(tokens.into_iter(), &mut symbol_table_builder)?;
    Ok(AST {
        program,
        symbol_table_builder,
    })
}

pub fn load(source: &str, mut symbol_table_builder: SymbolTableBuilder) -> Result<AST, ParseError> {
    let tokens = tokenize(source);
    let program = parse_program(tokens.into_iter(), &mut symbol_table_builder)?;
    Ok(AST {
        program,
        symbol_table_builder,
    })
}

fn parse_program<It>(
    mut curr_atom: It,
    symbol_table_builder: &mut SymbolTableBuilder,
) -> Result<Vec<SExpr>, ParseError>
where
    It: Iterator<Item = Token>,
{
    let mut prog = Vec::new();
    while let Some(atom) = curr_atom.next() {
        match atom {
            Token::LeftBracket => prog.push(parse_list(&mut curr_atom, symbol_table_builder)?),
            _ => return Err(ParseError::UnexpectedAtom),
        }
    }
    Ok(prog)
}

fn parse_expr<It>(
    curr_atom: &mut It,
    symbol_table_builder: &mut SymbolTableBuilder,
) -> Result<SExpr, ParseError>
where
    It: Iterator<Item = Token>,
{
    if let Some(atom) = curr_atom.next() {
        match atom {
            Token::LeftBracket => parse_list(curr_atom, symbol_table_builder),
            Token::Quote => parse_string(curr_atom),
            Token::Val(val) => parse_atom(val, symbol_table_builder),
            Token::SymbolQuote => parse_quote(curr_atom, symbol_table_builder),
            Token::QuasiQuote => parse_quasiquote(curr_atom, symbol_table_builder),
            Token::Unquote => parse_unquote(curr_atom, symbol_table_builder),
            _ => Err(ParseError::UnexpectedClosingParenthesis),
        }
    } else {
        Err(ParseError::UnexpectedEOF)
    }
}

fn parse_list<It>(
    curr_atom: &mut It,
    symbol_table_builder: &mut SymbolTableBuilder,
) -> Result<SExpr, ParseError>
where
    It: Iterator<Item = Token>,
{
    let mut list = Vec::new();
    while let Some(atom) = curr_atom.next() {
        list.push(match atom {
            Token::RightBracket => return Ok(SExpr::List(list)),
            Token::LeftBracket => parse_list(curr_atom, symbol_table_builder)?,
            Token::Quote => parse_string(curr_atom)?,
            Token::Val(val) => parse_atom(val, symbol_table_builder)?,
            Token::SymbolQuote => parse_quote(curr_atom, symbol_table_builder)?,
            Token::Unquote => parse_unquote(curr_atom, symbol_table_builder)?,
            Token::QuasiQuote => parse_quasiquote(curr_atom, symbol_table_builder)?,
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
    symbol_table_builder: &mut SymbolTableBuilder,
) -> Result<SExpr, ParseError>
where
    It: Iterator<Item = Token>,
{
    Ok(SExpr::List(vec![
        SExpr::Symbol(BuiltinSymbols::Quote as SymbolId),
        parse_expr(curr_atom, symbol_table_builder)?,
    ]))
}

fn parse_quasiquote<It>(
    curr_atom: &mut It,
    symbol_table_builder: &mut SymbolTableBuilder,
) -> Result<SExpr, ParseError>
where
    It: Iterator<Item = Token>,
{
    Ok(SExpr::List(vec![
        SExpr::Symbol(BuiltinSymbols::Quasiquote as SymbolId),
        parse_expr(curr_atom, symbol_table_builder)?,
    ]))
}

fn parse_unquote<It>(
    curr_atom: &mut It,
    symbol_table_builder: &mut SymbolTableBuilder,
) -> Result<SExpr, ParseError>
where
    It: Iterator<Item = Token>,
{
    Ok(SExpr::List(vec![
        SExpr::Symbol(BuiltinSymbols::Unquote as SymbolId),
        parse_expr(curr_atom, symbol_table_builder)?,
    ]))
}

fn parse_atom(
    curr_atom: String,
    symbol_table_builder: &mut SymbolTableBuilder,
) -> Result<SExpr, ParseError> {
    // `parse` resolves to `f64`.
    // It also handles negative numbers.
    match curr_atom.parse() {
        Ok(val) => Ok(SExpr::LitNumber(val)),
        Err(_) => Ok(SExpr::Symbol(symbol_table_builder.put_symbol(curr_atom))),
    }
}
