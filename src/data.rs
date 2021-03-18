use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    convert::TryFrom,
    rc::Rc,
};

pub type NativeFunc = Rc<dyn Fn(Environment, &SymbolTable, Vec<RuntimeVal>) -> RuntimeVal>;

pub struct RuntimeFunc {
    pub body: Box<SExpr>,
    pub name: SymbolId,
    pub args: Vec<SymbolId>,
}

impl RuntimeFunc {
    fn new(name: SymbolId, args: Vec<SymbolId>, body: SExpr) -> Rc<RuntimeFunc> {
        Rc::new(Self {
            name,
            args,
            body: Box::new(body),
        })
    }
}

#[derive(Clone)]
pub enum RuntimeVal {
    NumberVal(f64),
    StringVal(String),
    Symbol(SymbolId),
    List(Vec<RuntimeVal>),
    Func(Rc<RuntimeFunc>),
    NativeFunc(NativeFunc),
}

impl RuntimeVal {
    pub fn nil() -> RuntimeVal {
        RuntimeVal::List(Vec::new())
    }

    pub fn sym_true() -> RuntimeVal {
        RuntimeVal::Symbol(BuiltinSymbols::True as SymbolId)
    }

    pub fn predicate(val: bool) -> RuntimeVal {
        if val {
            Self::sym_true()
        } else {
            Self::sym_false()
        }
    }

    pub fn sym_false() -> RuntimeVal {
        RuntimeVal::Symbol(BuiltinSymbols::False as SymbolId)
    }

    pub fn function(name: SymbolId, args: Vec<SymbolId>, body: SExpr) -> RuntimeVal {
        Self::Func(RuntimeFunc::new(name, args, body))
    }

    pub fn native_function<Func>(func: Func) -> RuntimeVal
    where
        Func: 'static + Fn(Environment, &SymbolTable, Vec<RuntimeVal>) -> RuntimeVal,
    {
        RuntimeVal::NativeFunc(Rc::new(func))
    }

    pub fn repr(&self, symbol_table: &SymbolTable) -> String {
        use RuntimeVal::*;
        match self {
            NumberVal(val) => val.to_string(),
            StringVal(val) => format!(r#""{}""#, val),
            Symbol(val) => symbol_table[*val].clone(),
            List(vals) => {
                let mut res = String::from("(");
                vals.iter().for_each(|val| {
                    res += &val.repr(symbol_table);
                    res += " ";
                });
                res += ")";
                res
            }
            NativeFunc(_) => String::from("Native function object"),
            Func(_) => String::from("Function object"),
        }
    }

    pub fn str(&self, symbol_table: &SymbolTable) -> String {
        use RuntimeVal::*;
        match self {
            NumberVal(val) => val.to_string(),
            StringVal(val) => val.clone(),
            Symbol(val) => symbol_table[*val].clone(),
            list @ List(_) => list.repr(symbol_table),
            _ => panic!("No str representation"),
        }
    }
}

#[derive(Clone)]
pub enum SExpr {
    LitNumber(f64),
    LitString(String),
    Symbol(SymbolId),
    List(Vec<SExpr>),
}

pub struct EnvironmentImpl {
    pub parent: Option<Environment>,
    pub values: HashMap<SymbolId, RuntimeVal>,
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

    #[allow(dead_code)]
    pub fn with_parent(parent: Environment) -> Self {
        Self::wrap(EnvironmentImpl::with_parent(parent))
    }

    pub fn borrow(&self) -> Ref<EnvironmentImpl> {
        self.0.borrow()
    }

    pub fn borrow_mut(&mut self) -> RefMut<EnvironmentImpl> {
        self.0.borrow_mut()
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

pub type SymbolId = usize;
pub type SymbolTable = Vec<String>;

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

generate_builtin_symbols!{
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
        ("If": 10) => If,
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
