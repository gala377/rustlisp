use crate::{eval::{Interpreter, ModuleState}, runtime::WeakVal};
use std::{
    borrow::Borrow, cell::RefCell, collections::HashMap, convert::TryFrom, ops::Index, rc::Rc,
};

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
            // println!("Merging with symbol {}", key);
            self_map.entry(*key).or_insert(val.clone());
        });
    }

    /// Deep copies the whole environment tree.
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

    pub fn reparent(&mut self, parent: Environment) {
        self.borrow_mut().parent = Some(parent);
    }

    pub fn borrow(&self) -> std::cell::Ref<EnvironmentImpl> {
        (*self.0).borrow()
    }

    pub fn borrow_mut(&mut self) -> std::cell::RefMut<EnvironmentImpl> {
        (*self.0).borrow_mut()
    }

    pub fn into_parent(self) -> Option<Environment> {
        (*self.0).borrow().parent.clone()
    }
}

impl From<HashMap<SymbolId, WeakVal>> for Environment {
    fn from(values: HashMap<SymbolId, WeakVal>) -> Self {
        Self(Rc::new(RefCell::new(EnvironmentImpl {
            parent: None,
            values,
        })))
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


pub struct NativeModule {
    pub path: &'static str,
    pub items: HashMap<String, WeakVal>,
}

impl NativeModule {
    pub fn new(path: &'static str) -> Self {
        Self {
            path,
            items: Default::default(),
        }
    }

    pub fn with_items(path: &'static str, items: HashMap<String, WeakVal>) -> Self {
        Self { path, items }
    }

    pub fn insert_item(&mut self, key: impl Into<String>, val: WeakVal) {
        if let Some(_) = self.items.insert(key.into(), val) {
            panic!("Item already defined")
        }
    }

    pub fn add_into_vm(self, vm: &mut Interpreter) {
        match vm.modules.get_mut(self.path) {
            Some(ModuleState::Evaluating) => {
                panic!("Loading native module into currently evaluated module")
            }
            Some(ModuleState::Evaluated(ref mut env)) => {
                for (name, value) in self.items.into_iter() {
                    let sym_id = vm.symbols.put_symbol(&name);
                    env.borrow_mut().values.insert(sym_id, value);
                }
            }
            None => {
                let env: HashMap<SymbolId, WeakVal> = self
                    .items
                    .into_iter()
                    .map(|(name, val)| (vm.symbols.put_symbol(&name), val))
                    .collect();
                vm.modules.insert(
                    self.path.into(),
                    ModuleState::Evaluated(Environment::from(env)),
                );
            }
        }
    }
}

#[macro_export]
macro_rules! def_module {
    ($vm:ident, $module_name:literal, { $($name:literal => $func:expr ),+ $(,)?}) => {
        let mut module = $crate::env::NativeModule::new($module_name);
        $(
            module.insert_item($name, $crate::runtime::WeakVal::native_function($func))
        );+;
        module.add_into_vm($vm);
    }
}

/// Symbols Identifier.
/// Can be used to quickly compare symbols for identity.
pub type SymbolId = usize;

#[derive(Clone)]
pub struct SymbolTable {
    str_to_id: HashMap<String, SymbolId>,
    id_to_str: Vec<String>,
}

impl SymbolTable {
    pub fn get<Q>(&self, key: &Q) -> Option<SymbolId>
    where
        String: Borrow<Q>,
        Q: std::hash::Hash + Eq + ?Sized,
    {
        self.str_to_id.get(key).map(|x| *x)
    }
}

impl SymbolTable {
    pub fn builtin() -> Self {
        builtins_symbol_table()
    }

    pub fn put_symbol(&mut self, val: &str) -> SymbolId {
        match self.str_to_id.get(val) {
            Some(id) => *id,
            None => {
                self.id_to_str.push(val.into());
                self.str_to_id.insert(val.into(), self.id_to_str.len() - 1);
                self.id_to_str.len() - 1
            }
        }
    }
}

impl<Q> Index<&Q> for SymbolTable
where
    String: Borrow<Q>,
    Q: std::hash::Hash + Eq + ?Sized,
{
    type Output = SymbolId;

    fn index(&self, index: &Q) -> &Self::Output {
        self.str_to_id.get(index).unwrap()
    }
}

impl Index<usize> for SymbolTable {
    type Output = String;

    fn index(&self, index: usize) -> &Self::Output {
        &self.id_to_str[index]
    }
}

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
            SymbolTable{str_to_id: table, id_to_str: symbol_table}
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

        fn builtins_symbol_table() -> SymbolTable {
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
        ("none": 11) => None,
        ("set!": 12) => Set,
        ("macro": 13) => Macro,
        ("let": 14) => Let,
        ("unquote-splice": 15) => Splice,
        (".": 16) => Dot,
    }
}
