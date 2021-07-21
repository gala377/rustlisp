use crate::{
    check_ptr,
    data::BuiltinSymbols,
    data::{Environment, SymbolId, SymbolTable},
    eval::Interpreter,
    gc::{self, Allocable, Heap, HeapMarked, Root, TypeTag},
};
use std::rc::Rc;

pub struct Lambda {
    pub body: WeakVal,
    pub args: Vec<SymbolId>,
    pub env: Environment,
    pub globals: Environment,
}

impl Lambda {
    fn new(env: Environment, args: Vec<SymbolId>, body: WeakVal, globals: Environment) -> Self {
        Self {
            env,
            args,
            globals,
            body,
        }
    }
}

pub struct RuntimeFunc {
    pub body: WeakVal,
    pub name: SymbolId,
    pub args: Vec<SymbolId>,
    pub globals: Environment,
}

impl RuntimeFunc {
    fn new(
        name: SymbolId,
        args: Vec<SymbolId>,
        body: WeakVal,
        globals: Environment,
    ) -> RuntimeFunc {
        Self {
            name,
            args,
            globals,
            body,
        }
    }
}

pub type NativeFunc = Rc<dyn Fn(&mut Interpreter, Vec<RootedVal>) -> RootedVal>;

impl<F> Allocable for F
where
    F: Fn(Environment, &mut SymbolTable, Vec<RootedVal>, &mut Heap) -> RootedVal,
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
        TypeTag::String
    }
}

impl Allocable for Vec<WeakVal> {
    fn tag() -> TypeTag {
        TypeTag::List
    }
}

impl Allocable for RuntimeFunc {
    fn tag() -> TypeTag {
        TypeTag::RuntimeFunc
    }
}

pub enum RootedVal {
    // Copy types
    NumberVal(f64),
    StringVal(Root<std::string::String>),
    Symbol(SymbolId),
    List(Root<Vec<WeakVal>>),

    // Reference immutable types
    Func(Root<RuntimeFunc>),
    NativeFunc(NativeFunc),

    // Mutable reference types
    Lambda(Root<Lambda>),
}

impl RootedVal {
    pub fn heap_drop(self, heap: &mut Heap) {
        use RootedVal::*;
        match self {
            StringVal(x) => heap.drop_root(x),
            List(x) => heap.drop_root(x),
            Func(x) => heap.drop_root(x),
            Lambda(x) => heap.drop_root(x),
            NumberVal(_) | Symbol(_) | NativeFunc(_) => (),
        }
    }

    pub fn downgrade(self, heap: &mut Heap) -> WeakVal {
        use RootedVal::*;
        match self {
            NumberVal(x) => WeakVal::NumberVal(x),
            Symbol(x) => WeakVal::Symbol(x),
            StringVal(x) => WeakVal::StringVal(heap.downgrade(x)),
            List(x) => WeakVal::List(heap.downgrade(x)),
            Func(x) => WeakVal::Func(heap.downgrade(x)),
            NativeFunc(x) => WeakVal::NativeFunc(x),
            Lambda(x) => WeakVal::Lambda(heap.downgrade(x)),
        }
    }

    pub fn clone(&self, heap: &mut Heap) -> RootedVal {
        use RootedVal::*;
        match self {
            StringVal(x) => StringVal(heap.clone_root(x)),
            List(x) => List(heap.clone_root(x)),
            Func(x) => Func(heap.clone_root(x)),
            Lambda(x) => Lambda(heap.clone_root(x)),
            NumberVal(x) => NumberVal(*x),
            Symbol(x) => Symbol(*x),
            NativeFunc(x) => NativeFunc(Rc::clone(x)),
        }
    }

    pub fn nil(heap: &mut Heap) -> RootedVal {
        let inner = heap.allocate(Vec::new());
        RootedVal::List(inner)
    }

    pub fn sym_true() -> RootedVal {
        RootedVal::Symbol(BuiltinSymbols::True as SymbolId)
    }

    pub fn sym_false() -> RootedVal {
        RootedVal::Symbol(BuiltinSymbols::False as SymbolId)
    }

    pub fn predicate(val: bool) -> RootedVal {
        if val {
            Self::sym_true()
        } else {
            Self::sym_false()
        }
    }

    pub fn function(
        name: SymbolId,
        args: Vec<SymbolId>,
        body: WeakVal,
        globals: Environment,
        heap: &mut Heap,
    ) -> RootedVal {
        let inner = heap.allocate(RuntimeFunc::new(name, args, body, globals));
        Self::Func(inner)
    }

    pub fn lambda(
        env: Environment,
        args: Vec<SymbolId>,
        body: WeakVal,
        globals: Environment,
        heap: &mut Heap,
    ) -> RootedVal {
        let inner = heap.allocate(Lambda::new(env, args, body, globals));
        Self::Lambda(inner)
    }

    pub fn string(val: std::string::String, heap: &mut Heap) -> RootedVal {
        let inner = heap.allocate(val);
        Self::StringVal(inner)
    }

    pub fn list_from_rooted(val: Vec<RootedVal>, heap: &mut Heap) -> RootedVal {
        let mut inner: Root<Vec<WeakVal>> = heap.allocate(Vec::new());
        check_ptr!(heap, inner);
        let val: Vec<WeakVal> = val.into_iter().map(|x| x.downgrade(heap)).collect();
        heap.deref_mut(&mut inner).extend(val.into_iter());
        Self::List(inner)
    }

    pub fn native_function<Func>(func: Func) -> RootedVal
    where
        Func: 'static + Fn(&mut Interpreter, Vec<RootedVal>) -> RootedVal,
    {
        RootedVal::NativeFunc(Rc::new(func))
    }

    pub fn repr(&self, heap: &Heap, symbol_table: &SymbolTable) -> std::string::String {
        use RootedVal::*;
        match self {
            NumberVal(val) => val.to_string(),
            StringVal(val) => format!(r#""{}""#, *heap.deref(val)),
            Symbol(val) => symbol_table[*val].clone(),
            List(vals) => {
                let mut res = std::string::String::from("(");
                heap.deref(vals).iter().for_each(|val| {
                    res += &val.repr(heap, symbol_table);
                    res += " ";
                });
                res += ")";
                res
            }
            NativeFunc(_) => std::string::String::from("Native function"),
            Func(func) => {
                let symbol = heap.deref(func).name;
                format!("Function {}", symbol_table[symbol])
            }
            Lambda(_) => std::string::String::from("Lambda object"),
        }
    }

    pub fn str(&self, heap: &Heap, symbol_table: &SymbolTable) -> std::string::String {
        use RootedVal::*;
        match self {
            NumberVal(val) => val.to_string(),
            StringVal(val) => heap.deref(val).clone(),
            Symbol(val) => symbol_table[*val].clone(),
            list @ List(_) => list.repr(heap, symbol_table),
            _ => panic!("No str representation"),
        }
    }
}

#[derive(Clone)]
pub enum WeakVal {
    NumberVal(f64),
    Symbol(SymbolId),
    StringVal(gc::Weak<std::string::String>),
    List(gc::Weak<Vec<WeakVal>>),
    Func(gc::Weak<RuntimeFunc>),
    NativeFunc(NativeFunc),
    Lambda(gc::Weak<Lambda>),
}

impl WeakVal {
    pub fn upgrade(self, heap: &mut Heap) -> RootedVal {
        use WeakVal::*;
        match self {
            NumberVal(x) => RootedVal::NumberVal(x),
            Symbol(x) => RootedVal::Symbol(x),
            StringVal(x) => RootedVal::StringVal(heap.upgrade(x)),
            List(x) => RootedVal::List(heap.upgrade(x)),
            Func(x) => RootedVal::Func(heap.upgrade(x)),
            NativeFunc(x) => RootedVal::NativeFunc(x),
            Lambda(x) => RootedVal::Lambda(heap.upgrade(x)),
        }
    }

    pub fn native_function<Func>(func: Func) -> WeakVal
    where
        Func: 'static + Fn(&mut Interpreter, Vec<RootedVal>) -> RootedVal,
    {
        Self::NativeFunc(Rc::new(func))
    }

    pub fn repr(&self, heap: &Heap, symbol_table: &SymbolTable) -> std::string::String {
        use WeakVal::*;
        match self {
            NumberVal(val) => val.to_string(),
            StringVal(val) => format!(r#""{}""#, *heap.deref(val)),
            Symbol(val) => symbol_table[*val].clone(),
            List(vals) => {
                let mut res = std::string::String::from("(");
                heap.deref(vals).iter().for_each(|val| {
                    res += &val.repr(heap, symbol_table);
                    res += " ";
                });
                res += ")";
                res
            }
            NativeFunc(_) => std::string::String::from("Native function"),
            Func(func) => {
                let symbol = heap.deref(func).name;
                format!("Function {}", symbol_table[symbol])
            }
            Lambda(_) => std::string::String::from("Lambda object"),
        }
    }

    pub fn str(&self, heap: &Heap, symbol_table: &SymbolTable) -> std::string::String {
        use WeakVal::*;
        match self {
            NumberVal(val) => val.to_string(),
            StringVal(val) => heap.deref(val).clone(),
            Symbol(val) => symbol_table[*val].clone(),
            list @ List(_) => list.repr(heap, symbol_table),
            NativeFunc(_) => "Native function".to_owned(),
            Func(func) => {
                let name = &symbol_table[heap.deref(func).name];
                format!("Runtime function {}", name)
            }
            Lambda(_) => "Lambda function".to_owned(),
        }
    }

    pub fn simple_repr(&self, heap: &Heap) -> std::string::String {
        use WeakVal::*;
        match self {
            NumberVal(val) => val.to_string(),
            StringVal(val) => heap.deref(val).clone(),
            Symbol(val) => val.to_string(),
            List(vals) => {
                let mut res = std::string::String::from("(");
                heap.deref(vals).iter().for_each(|val| {
                    res += &val.simple_repr(heap);
                    res += " ";
                });
                res += ")";
                res
            }
            NativeFunc(_) => "Native function".to_owned(),
            Func(func) => {
                let name = heap.deref(func).name;
                format!("Runtime function {}", name)
            }
            Lambda(_) => "Lambda function".to_owned(),
        }
    }
}

pub fn drop_rooted_vec(heap: &mut Heap, vec: Vec<RootedVal>) {
    vec.into_iter().for_each(|x| x.heap_drop(heap));
}
