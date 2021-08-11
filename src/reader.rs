use crate::{
    env::{BuiltinSymbols, SymbolId, SymbolTable},
    gc::Heap,
    runtime::RootedVal,
};

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
    Splice,
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
            ",@" => Splice,
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
        .replace('@', " @ ")
        .replace(" ,  @ ", " ,@ ")
        .split_whitespace()
        .map(Token::from)
        .collect()
}

/// Abstract representation of an unevaluated program as a tree structure.
/// Program is represented as a sequence of s-expressions to evaluate in sequence.
/// Additional symbol table is provided with all the symbols that were found
/// during parsing and all symbols denoting special forms.
pub struct AST {
    pub program: Vec<RootedVal>,
}

impl From<Vec<RootedVal>> for AST {
    fn from(program: Vec<RootedVal>) -> Self {
        Self { program }
    }
}

pub fn read(
    source: &str,
    heap: &mut Heap,
    symbol_table: &mut SymbolTable,
) -> Result<AST, ParseError> {
    let tokens = tokenize(source);
    let program = parse_program(tokens.into_iter(), heap, symbol_table)?;
    Ok(program.into())
}

fn parse_program<It>(
    mut curr_atom: It,
    heap: &mut Heap,
    symbol_table: &mut SymbolTable,
) -> Result<Vec<RootedVal>, ParseError>
where
    It: Iterator<Item = Token>,
{
    let mut prog = Vec::new();
    while let Some(atom) = curr_atom.next() {
        match atom {
            Token::LeftBracket => prog.push(parse_list(&mut curr_atom, heap, symbol_table)?),
            _ => return Err(ParseError::UnexpectedAtom),
        }
    }
    Ok(prog)
}

fn parse_expr<It>(
    curr_atom: &mut It,
    heap: &mut Heap,
    symbol_table: &mut SymbolTable,
) -> Result<RootedVal, ParseError>
where
    It: Iterator<Item = Token>,
{
    if let Some(atom) = curr_atom.next() {
        match atom {
            Token::LeftBracket => parse_list(curr_atom, heap, symbol_table),
            Token::Quote => parse_string(curr_atom, heap),
            Token::Val(val) => parse_atom(val, symbol_table),
            Token::SymbolQuote => parse_quote(curr_atom, heap, symbol_table),
            Token::QuasiQuote => parse_quasiquote(curr_atom, heap, symbol_table),
            Token::Unquote => parse_unquote(curr_atom, heap, symbol_table),
            Token::Splice => parese_splice(curr_atom, heap, symbol_table),
            _ => Err(ParseError::UnexpectedClosingParenthesis),
        }
    } else {
        Err(ParseError::UnexpectedEOF)
    }
}

fn parse_list<It>(
    curr_atom: &mut It,
    heap: &mut Heap,
    symbol_table: &mut SymbolTable,
) -> Result<RootedVal, ParseError>
where
    It: Iterator<Item = Token>,
{
    let mut list = Vec::new();
    while let Some(atom) = curr_atom.next() {
        list.push(match atom {
            Token::RightBracket => return Ok(RootedVal::list_from_rooted(list, heap)),
            Token::LeftBracket => parse_list(curr_atom, heap, symbol_table)?,
            Token::Quote => parse_string(curr_atom, heap)?,
            Token::Val(val) => parse_atom(val, symbol_table)?,
            Token::SymbolQuote => parse_quote(curr_atom, heap, symbol_table)?,
            Token::Unquote => parse_unquote(curr_atom, heap, symbol_table)?,
            Token::QuasiQuote => parse_quasiquote(curr_atom, heap, symbol_table)?,
            Token::Splice => parese_splice(curr_atom, heap, symbol_table)?,
        });
    }
    Err(ParseError::UnclosedList)
}

fn parse_string<It>(curr_atom: &mut It, heap: &mut Heap) -> Result<RootedVal, ParseError>
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
            Token::Splice => res += ",@",
        };
        res.push(' ');
    }
    Ok(RootedVal::string(res, heap))
}

fn parse_quote<It>(
    curr_atom: &mut It,
    heap: &mut Heap,
    symbol_table: &mut SymbolTable,
) -> Result<RootedVal, ParseError>
where
    It: Iterator<Item = Token>,
{
    Ok(RootedVal::list_from_rooted(
        vec![
            RootedVal::Symbol(BuiltinSymbols::Quote as SymbolId),
            parse_expr(curr_atom, heap, symbol_table)?,
        ],
        heap,
    ))
}

fn parse_quasiquote<It>(
    curr_atom: &mut It,
    heap: &mut Heap,
    symbol_table: &mut SymbolTable,
) -> Result<RootedVal, ParseError>
where
    It: Iterator<Item = Token>,
{
    Ok(RootedVal::list_from_rooted(
        vec![
            RootedVal::Symbol(BuiltinSymbols::Quasiquote as SymbolId),
            parse_expr(curr_atom, heap, symbol_table)?,
        ],
        heap,
    ))
}

fn parse_unquote<It>(
    curr_atom: &mut It,
    heap: &mut Heap,
    symbol_table: &mut SymbolTable,
) -> Result<RootedVal, ParseError>
where
    It: Iterator<Item = Token>,
{
    Ok(RootedVal::list_from_rooted(
        vec![
            RootedVal::Symbol(BuiltinSymbols::Unquote as SymbolId),
            parse_expr(curr_atom, heap, symbol_table)?,
        ],
        heap,
    ))
}

fn parese_splice<It>(
    curr_atom: &mut It,
    heap: &mut Heap,
    symbol_table: &mut SymbolTable,
) -> Result<RootedVal, ParseError>
where
    It: Iterator<Item = Token>,
{
    Ok(RootedVal::list_from_rooted(
        vec![
            RootedVal::Symbol(BuiltinSymbols::Splice as SymbolId),
            parse_expr(curr_atom, heap, symbol_table)?,
        ],
        heap,
    ))
}

fn parse_atom(curr_atom: String, symbol_table: &mut SymbolTable) -> Result<RootedVal, ParseError> {
    // `parse` resolves to `f64`.
    // It also handles negative numbers.
    match curr_atom.parse() {
        Ok(val) => Ok(RootedVal::NumberVal(val)),
        Err(_) => Ok(RootedVal::Symbol(symbol_table.put_symbol(curr_atom))),
    }
}
