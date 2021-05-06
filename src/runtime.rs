use std::rc::Rc;

use crate::data::BuiltinSymbols;
use crate::{
    data::{Environment, SExpr, SymbolId, SymbolTable},
    gc::{Allocable, Gc, Heap, TypeTag},
};

pub struct Lambda {
    pub body: Box<SExpr>,
    pub args: Vec<SymbolId>,
    pub env: Environment,
}

impl Lambda {
    fn new(env: Environment, args: Vec<SymbolId>, body: SExpr) -> Self {
        Self {
            env,
            args,
            body: Box::new(body),
        }
    }
}

pub struct String {
    pub data: std::string::String,
}

impl String {
    pub fn new() -> Self {
        Self {
            data: std::string::String::new(),
        }
    }
}

pub struct List {
    pub data: Vec<RuntimeVal>,
}

impl List {
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }
}

pub struct RuntimeFunc {
    pub body: Box<SExpr>,
    pub name: SymbolId,
    pub args: Vec<SymbolId>,
}

impl RuntimeFunc {
    fn new(name: SymbolId, args: Vec<SymbolId>, body: SExpr) -> RuntimeFunc {
        Self {
            name,
            args,
            body: Box::new(body),
        }
    }
}

pub type NativeFunc =
    Rc<dyn Fn(&mut Heap, Environment, &mut SymbolTable, Vec<RuntimeVal>) -> RuntimeVal>;

impl<F> Allocable for F
where
    F: Fn(Environment, &mut SymbolTable, Vec<RuntimeVal>, &mut Heap) -> RuntimeVal,
{
    fn tag() -> TypeTag {
        TypeTag::None
    }
}

impl Allocable for Lambda {
    fn tag() -> TypeTag {
        TypeTag::Lambda
    }
}

impl Allocable for String {
    fn tag() -> TypeTag {
        TypeTag::None
    }
}

impl Allocable for List {
    fn tag() -> TypeTag {
        TypeTag::List
    }
}

impl<T> Allocable for Vec<T> {
    fn tag() -> TypeTag {
        TypeTag::List
    }
}

impl Allocable for std::string::String {
    fn tag() -> TypeTag {
        TypeTag::None
    }
}

impl Allocable for RuntimeFunc {
    fn tag() -> TypeTag {
        TypeTag::None
    }
}

#[derive(Clone)]
pub enum RuntimeVal {
    // Copy types
    NumberVal(f64),
    StringVal(Gc<std::string::String>),
    Symbol(SymbolId),
    List(Gc<Vec<RuntimeVal>>),

    // Reference immutable types
    Func(Gc<RuntimeFunc>),
    NativeFunc(NativeFunc),

    // Mutable reference types
    Lambda(Gc<Lambda>),
}

impl RuntimeVal {
    pub fn nil(heap: &mut Heap) -> RuntimeVal {
        let inner = heap.allocate(Vec::new());
        RuntimeVal::List(inner)
    }

    pub fn sym_true() -> RuntimeVal {
        RuntimeVal::Symbol(BuiltinSymbols::True as SymbolId)
    }

    pub fn sym_false() -> RuntimeVal {
        RuntimeVal::Symbol(BuiltinSymbols::False as SymbolId)
    }

    pub fn predicate(val: bool) -> RuntimeVal {
        if val {
            Self::sym_true()
        } else {
            Self::sym_false()
        }
    }

    pub fn function(
        name: SymbolId,
        args: Vec<SymbolId>,
        body: SExpr,
        heap: &mut Heap,
    ) -> RuntimeVal {
        let inner = heap.allocate(RuntimeFunc::new(name, args, body));
        Self::Func(inner)
    }

    pub fn lambda(
        env: Environment,
        args: Vec<SymbolId>,
        body: SExpr,
        heap: &mut Heap,
    ) -> RuntimeVal {
        let inner = heap.allocate(Lambda::new(env, args, body));
        Self::Lambda(inner)
    }

    pub fn string(val: std::string::String, heap: &mut Heap) -> RuntimeVal {
        let inner = heap.allocate(val);
        Self::StringVal(inner)
    }

    pub fn list(val: Vec<RuntimeVal>, heap: &mut Heap) -> RuntimeVal {
        let inner = heap.allocate(val);
        Self::List(inner)
    }

    pub fn native_function<Func>(func: Func) -> RuntimeVal
    where
        Func: 'static + Fn(&mut Heap, Environment, &mut SymbolTable, Vec<RuntimeVal>) -> RuntimeVal,
    {
        RuntimeVal::NativeFunc(Rc::new(func))
    }

    pub fn repr(&self, symbol_table: &SymbolTable) -> std::string::String {
        use RuntimeVal::*;
        match self {
            NumberVal(val) => val.to_string(),
            StringVal(val) => format!(r#""{}""#, unsafe { val.data.as_ref() }),
            Symbol(val) => symbol_table[*val].clone(),
            List(vals) => {
                let mut res = std::string::String::from("(");
                unsafe {
                    vals.data.as_ref().iter().for_each(|val| {
                        res += &val.repr(symbol_table);
                        res += " ";
                    });
                }
                res += ")";
                res
            }
            NativeFunc(_) => std::string::String::from("Native function"),
            Func(func) => {
                let symbol = unsafe { func.data.as_ref().name };
                format!("Function {}", symbol_table[symbol])
            }
            Lambda(_) => std::string::String::from("Lambda object"),
        }
    }

    pub fn str(&self, symbol_table: &SymbolTable) -> std::string::String {
        use RuntimeVal::*;
        match self {
            NumberVal(val) => val.to_string(),
            StringVal(val) => unsafe { val.data.as_ref().clone() },
            Symbol(val) => symbol_table[*val].clone(),
            list @ List(_) => list.repr(symbol_table),
            _ => panic!("No str representation"),
        }
    }
}
