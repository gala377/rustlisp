use crate::{data::{BuiltinSymbols, SymbolId, SymbolTableBuilder}, gc::Heap, runtime::{self, RootedVal, WeakVal}};

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
    pub program: Vec<RootedVal>,
    pub symbol_table_builder: SymbolTableBuilder,
}

/// Parses source code passed as a string.
/// Successful parsing returns unevaluated AST struct.
pub fn read(source: &str, heap: &mut Heap) -> Result<AST, ParseError> {
    let tokens = tokenize(source);
    let mut symbol_table_builder = SymbolTableBuilder::builtin();
    let program = parse_program(tokens.into_iter(), heap, &mut symbol_table_builder)?;
    Ok(AST {
        program,
        symbol_table_builder,
    })
}

pub fn load(source: &str, heap: &mut Heap, mut symbol_table_builder: SymbolTableBuilder) -> Result<AST, ParseError> {
    let tokens = tokenize(source);
    let program = parse_program(tokens.into_iter(), heap, &mut symbol_table_builder)?;
    Ok(AST {
        program,
        symbol_table_builder,
    })
}

fn parse_program<It>(
    mut curr_atom: It,
    heap: &mut Heap,
    symbol_table_builder: &mut SymbolTableBuilder,
) -> Result<Vec<RootedVal>, ParseError>
where
    It: Iterator<Item = Token>,
{
    let mut prog = Vec::new();
    while let Some(atom) = curr_atom.next() {
        match atom {
            Token::LeftBracket => prog.push(parse_list(&mut curr_atom, heap, symbol_table_builder)?),
            _ => return Err(ParseError::UnexpectedAtom),
        }
    }
    Ok(prog)
}

fn parse_expr<It>(
    curr_atom: &mut It,
    heap: &mut Heap,
    symbol_table_builder: &mut SymbolTableBuilder,
) -> Result<RootedVal, ParseError>
where
    It: Iterator<Item = Token>,
{
    if let Some(atom) = curr_atom.next() {
        match atom {
            Token::LeftBracket => parse_list(curr_atom, heap, symbol_table_builder),
            Token::Quote => parse_string(curr_atom, heap),
            Token::Val(val) => parse_atom(val, symbol_table_builder),
            Token::SymbolQuote => parse_quote(curr_atom, heap, symbol_table_builder),
            Token::QuasiQuote => parse_quasiquote(curr_atom, heap, symbol_table_builder),
            Token::Unquote => parse_unquote(curr_atom, heap, symbol_table_builder),
            _ => Err(ParseError::UnexpectedClosingParenthesis),
        }
    } else {
        Err(ParseError::UnexpectedEOF)
    }
}

fn parse_list<It>(
    curr_atom: &mut It,
    heap: &mut Heap,
    symbol_table_builder: &mut SymbolTableBuilder,
) -> Result<RootedVal, ParseError>
where
    It: Iterator<Item = Token>,
{
    let mut list = Vec::new();
    while let Some(atom) = curr_atom.next() {
        list.push(match atom {
            Token::RightBracket => return Ok(RootedVal::list_from_rooted(list, heap)),
            Token::LeftBracket => parse_list(curr_atom, heap, symbol_table_builder)?,
            Token::Quote => parse_string(curr_atom, heap)?,
            Token::Val(val) => parse_atom(val, symbol_table_builder)?,
            Token::SymbolQuote => parse_quote(curr_atom, heap, symbol_table_builder)?,
            Token::Unquote => parse_unquote(curr_atom, heap, symbol_table_builder)?,
            Token::QuasiQuote => parse_quasiquote(curr_atom, heap, symbol_table_builder)?,
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
        };
        res.push(' ');
    }
    Ok(RootedVal::string(res, heap))
}

fn parse_quote<It>(
    curr_atom: &mut It,
    heap: &mut Heap,
    symbol_table_builder: &mut SymbolTableBuilder,
) -> Result<RootedVal, ParseError>
where
    It: Iterator<Item = Token>,
{
    Ok(RootedVal::list_from_rooted(vec![
        RootedVal::Symbol(BuiltinSymbols::Quote as SymbolId),
        parse_expr(curr_atom, heap, symbol_table_builder)?,
    ], heap))
}

fn parse_quasiquote<It>(
    curr_atom: &mut It,
    heap: &mut Heap,
    symbol_table_builder: &mut SymbolTableBuilder,
) -> Result<RootedVal, ParseError>
where
    It: Iterator<Item = Token>,
{
    Ok(RootedVal::list_from_rooted(vec![
        RootedVal::Symbol(BuiltinSymbols::Quasiquote as SymbolId),
        parse_expr(curr_atom, heap, symbol_table_builder)?,
    ], heap))
}

fn parse_unquote<It>(
    curr_atom: &mut It,
    heap: &mut Heap,
    symbol_table_builder: &mut SymbolTableBuilder,
) -> Result<RootedVal, ParseError>
where
    It: Iterator<Item = Token>,
{
    Ok(RootedVal::list_from_rooted(vec![
        RootedVal::Symbol(BuiltinSymbols::Unquote as SymbolId),
        parse_expr(curr_atom, heap, symbol_table_builder)?,
    ], heap))
}

fn parse_atom(
    curr_atom: String,
    symbol_table_builder: &mut SymbolTableBuilder,
) -> Result<RootedVal, ParseError> {
    // `parse` resolves to `f64`.
    // It also handles negative numbers.
    match curr_atom.parse() {
        Ok(val) => Ok(RootedVal::NumberVal(val)),
        Err(_) => Ok(RootedVal::Symbol(symbol_table_builder.put_symbol(curr_atom))),
    }
}
