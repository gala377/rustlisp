use crate::runtime::WeakVal;
use std::{cell::RefCell, collections::HashMap, convert::TryFrom, rc::Rc};

#[derive(Clone)]
pub enum SExpr {
    LitNumber(f64),
    LitString(String),
    Symbol(SymbolId),
    List(Vec<SExpr>),
}

pub struct EnvironmentImpl {
    pub parent: Option<Environment>,
    pub values: HashMap<SymbolId, WeakVal>,
}

#[derive(Clone)]
pub struct Environment(Rc<RefCell<EnvironmentImpl>>);

impl Environment {
    pub fn wrap(inner: EnvironmentImpl) -> Self {
        Self(Rc::new(RefCell::new(inner)))
    }

    pub fn new() -> Self {
        Self::wrap(EnvironmentImpl::new())
    }

    pub fn update_with(&mut self, other: Environment) {
        let inner = other.borrow();
        let self_map = &mut self.borrow_mut().values;
        inner.values.iter().for_each(|(key, val)| {
            println!("Merging with symbol {}", key);
            self_map.entry(*key).or_insert(val.clone());
        });
    }

    pub fn split(self) -> Environment {
        let inner = self.borrow();
        Self(Rc::new(RefCell::new(EnvironmentImpl {
            parent: inner.parent.clone().map(Self::split),
            values: inner.values.clone(),
        })))
    }

    pub fn with_parent(parent: Environment) -> Self {
        Self::wrap(EnvironmentImpl::with_parent(parent))
    }

    pub fn borrow(&self) -> std::cell::Ref<EnvironmentImpl> {
        self.0.borrow()
    }

    pub fn borrow_mut(&mut self) -> std::cell::RefMut<EnvironmentImpl> {
        (*self.0).borrow_mut()
    }

    pub fn into_parent(self) -> Option<Environment> {
        self.0.borrow().parent.clone()
    }
}

impl EnvironmentImpl {
    pub fn new() -> EnvironmentImpl {
        Self {
            parent: None,
            values: HashMap::new(),
        }
    }

    pub fn with_parent(parent: Environment) -> EnvironmentImpl {
        Self {
            parent: Some(parent),
            values: HashMap::new(),
        }
    }
}

/// Symbols Identifier.
/// Can be used to quickly compare symbols for identity.
pub type SymbolId = usize;

/// An associating table mapping SymbolId to its string representation.
pub type SymbolTable = Vec<String>;

/// Creates symbol table with provided symbols as well as
/// reverse mapping from string to `SymbolId`
///
/// # Syntax
///
/// Body of a macro consists of one or more arms.
/// Each arm has a form `$name => $val`.
/// `$name` should be a `str` literal and `$val` should be a `SymbolId`.
///
/// # Returns
///
/// A tuple string to symbol mapping and a mapping from symbol to string.
/// The first one is a `HashMap<String, SymbolId>` and
/// the second one is a `SymbolTable`.
///
/// # Examples
///
/// ```
/// let (string_to_sym, sym_to_string) = build_sym_table!{
///     "define" => 0,
///     "not" => 1,
/// };
/// assert_eq!(string_to_sym.get(sym_to_string[0]).unwrap(), 0);
/// ```
macro_rules! build_sym_table {
    ($($name:literal => $val:expr),*$(,)?) => {
        {
            let mut table = HashMap::new();
            {$(
                table.insert(String::from($name), $val as usize);
            )*};
            let mut symbol_table = Vec::new();
            {$(
                symbol_table.push(String::from($name));
            )*};
            (table, symbol_table)
        }
    };
}

/// Provides convenient syntax to predefine symbols before parsing.
/// For example could be used to predefine special form symbols for
/// easier reference during evaluation.
///
/// # Syntax
///
/// Syntax looks like defining a special enum.
/// Each enum variant is in form `($pattern: $id) => $variant,`
/// where `$pattern` is a literal string representing a symbol in source,
/// `$id` should be an incrementing integer, starting from zero,
/// corresponding to the `SymbolId` of the symbol and `$variant` is a variants identifier
/// in the enums definition.
///
/// The enum then will be generated with the provided name and the provided variants.
/// Additionally the enum will provide `try_from` method to convert `SymbolId` to itself.
///
/// # Generates
///
/// Enum with provided name and variants corresponding to defined variants.
///
/// `builtin_symbol_table` convenience function returning mappings
/// from string to symbol and reverse, filled with defined symbols.
////
/// # Examples
///
/// ```
///   generate_builtin_symbols! {
///       BuiltinSymbols { // name of the generated enum
///          ("define": 0) => Define, // `define` variant
///          ("not": 1) => Not, // `not` variant
///       }
///   }
///
///   let (str_to_sym, sym_to_str) = builtin_symbol_table();
///   let sym = str_to_sym.get(&"not").unwrap();
///   match (*sym).try_into() {
///      BuiltinSymbols::Not => (),
///      _ => panic!("expected not symbol"),
///   }
/// ```
macro_rules! generate_builtin_symbols {
    ($enum_name:ident {$(( $name:literal: $num:literal ) => $sym:tt),*$(,)?}) => {
        pub enum $enum_name {
            $(
                $sym,
            )*
        }

        impl TryFrom<SymbolId> for $enum_name {
            type Error = &'static str;
            fn try_from(value: SymbolId) -> Result<Self, Self::Error> {
                use $enum_name::*;
                match value {
                    $($num => Ok($sym),)*
                    _ => Err("out of range"),
                }
            }
        }

        fn builtins_symbol_table() -> (HashMap<String, SymbolId>, SymbolTable) {
            use $enum_name::*;
            build_sym_table!{
                $(
                    $name => $sym,
                )*
            }
        }
    };
}

generate_builtin_symbols! {
    BuiltinSymbols {
        ("def": 0) => Define,
        ("quote": 1) => Quote,
        ("unquote": 2) => Unquote,
        ("quasiquote": 3) => Quasiquote,
        ("begin": 4) => Begin,
        ("#t": 5) => True,
        ("#f": 6) => False,
        ("cond": 7) => Cond,
        ("lambda": 8) => Lambda,
        ("while": 9) => While,
        ("if": 10) => If,
    }
}

pub struct SymbolTableBuilder {
    symbols: HashMap<String, SymbolId>,
    symbol_table: SymbolTable,
}

impl SymbolTableBuilder {
    pub fn builtin() -> Self {
        let (symbols, symbol_table) = builtins_symbol_table();
        Self {
            symbols,
            symbol_table,
        }
    }

    pub fn with_symbols(symbols: &SymbolTable) -> Self {
        let mut map = HashMap::new();
        for (i, val) in symbols.iter().enumerate() {
            map.insert(val.clone(), i);
        }
        Self {
            symbols: map,
            symbol_table: symbols.clone(),
        }
    }

    pub fn update_table(&self, table: &mut SymbolTable) {
        table.clone_from(&self.symbol_table);
    }

    pub fn build(self) -> SymbolTable {
        self.symbol_table
    }

    pub fn put_symbol(&mut self, val: String) -> SymbolId {
        match self.symbols.get(&val) {
            Some(id) => *id,
            None => {
                self.symbol_table.push(val.clone());
                self.symbols.insert(val, self.symbol_table.len() - 1);
                self.symbol_table.len() - 1
            }
        }
    }
}
