use crate::{
    check_ptr,
    env::BuiltinSymbols,
    env::{Environment, SymbolId, SymbolTable},
    eval::Interpreter,
    gc::{self, Allocable, Heap, HeapMarked, Root, TypeTag},
    native::{NativeStruct, RootedStructPtr, WeakStructPtr},
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

#[derive(Clone)]
pub enum RootedVal {
    // Copy types
    NumberVal(f64),
    StringVal(Root<std::string::String>),
    Symbol(SymbolId),

    // Reference immutable types
    Func(Root<RuntimeFunc>),
    NativeFunc(NativeFunc),

    // Mutable reference types
    Lambda(Root<Lambda>),
    List(Root<Vec<WeakVal>>),
    UserType(RootedStructPtr),
}

impl RootedVal {
    pub fn downgrade(self) -> WeakVal {
        use RootedVal::*;
        match self {
            NumberVal(x) => WeakVal::NumberVal(x),
            Symbol(x) => WeakVal::Symbol(x),
            StringVal(x) => WeakVal::StringVal(x.downgrade()),
            List(x) => WeakVal::List(x.downgrade()),
            Func(x) => WeakVal::Func(x.downgrade()),
            NativeFunc(x) => WeakVal::NativeFunc(x),
            UserType(x) => WeakVal::UserType(x.downgrade()),
            Lambda(x) => WeakVal::Lambda(x.downgrade()),
        }
    }

    pub fn as_weak(&self) -> WeakVal {
        self.clone().downgrade()
    }

    pub fn nil(heap: &mut Heap) -> RootedVal {
        let inner = heap.allocate(Vec::new());
        RootedVal::List(inner)
    }

    pub fn none() -> RootedVal {
        RootedVal::Symbol(BuiltinSymbols::None as SymbolId)
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

    pub fn user_type<T: NativeStruct + Allocable>(val: T, heap: &mut Heap) -> RootedVal {
        let ptr = RootedStructPtr::new(val, heap);
        RootedVal::UserType(ptr)
    }

    pub fn list(val: Vec<WeakVal>, heap: &mut Heap) -> RootedVal {
        let inner = heap.allocate(val);
        Self::List(inner)
    }

    pub fn list_from_rooted(val: Vec<RootedVal>, heap: &mut Heap) -> RootedVal {
        let mut inner: Root<Vec<WeakVal>> = heap.allocate(Vec::new());
        check_ptr!(heap, inner);
        let val: Vec<WeakVal> = val.into_iter().map(|x| x.downgrade()).collect();
        heap.deref_ptr_mut(&mut inner).extend(val.into_iter());
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
            StringVal(val) => format!(r#""{}""#, *heap.deref_ptr(val)),
            Symbol(val) => symbol_table[*val].clone(),
            List(vals) => {
                let mut res = std::string::String::from("(");
                heap.deref_ptr(vals).iter().for_each(|val| {
                    res += &val.repr(heap, symbol_table);
                    res += " ";
                });
                res += ")";
                res
            }
            NativeFunc(_) => std::string::String::from("Native function"),
            Func(func) => {
                let symbol = heap.deref_ptr(func).name;
                format!("Function {}", symbol_table[symbol])
            }
            Lambda(_) => std::string::String::from("Lambda object"),
            _ => "No representation".into(),
        }
    }

    pub fn str(&self, heap: &Heap, symbol_table: &SymbolTable) -> std::string::String {
        use RootedVal::*;
        match self {
            NumberVal(val) => val.to_string(),
            StringVal(val) => heap.deref_ptr(val).clone(),
            Symbol(val) => symbol_table[*val].clone(),
            list @ List(_) => list.repr(heap, symbol_table),
            _ => panic!("No str representation"),
        }
    }

    pub fn is_symbol(&self, sym: SymbolId) -> bool {
        match self {
            RootedVal::Symbol(inner) => *inner == sym,
            _ => false,
        }
    }

    pub fn is_callable(&self) -> bool {
        match self {
            Self::Func(_) | Self::NativeFunc(_) | Self::Lambda(_) => true,
            _ => false,
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
    UserType(WeakStructPtr),
}

impl WeakVal {
    pub fn upgrade(self) -> RootedVal {
        use WeakVal::*;
        match self {
            NumberVal(x) => RootedVal::NumberVal(x),
            Symbol(x) => RootedVal::Symbol(x),
            StringVal(x) => RootedVal::StringVal(x.upgrade()),
            List(x) => RootedVal::List(x.upgrade()),
            Func(x) => RootedVal::Func(x.upgrade()),
            NativeFunc(x) => RootedVal::NativeFunc(x),
            Lambda(x) => RootedVal::Lambda(x.upgrade()),
            UserType(x) => RootedVal::UserType(x.upgrade()),
        }
    }

    pub fn native_function<Func>(func: Func) -> WeakVal
    where
        Func: 'static + Fn(&mut Interpreter, Vec<RootedVal>) -> RootedVal,
    {
        Self::NativeFunc(Rc::new(func))
    }

    pub fn is_symbol(&self, sym: SymbolId) -> bool {
        match self {
            WeakVal::Symbol(inner) if *inner == sym => true,
            _ => false,
        }
    }

    pub fn repr(&self, heap: &Heap, symbol_table: &SymbolTable) -> std::string::String {
        use WeakVal::*;
        match self {
            NumberVal(val) => val.to_string(),
            StringVal(val) => format!(r#""{}""#, *heap.deref_ptr(val)),
            Symbol(val) => symbol_table[*val].clone(),
            List(vals) => {
                let mut res = std::string::String::from("(");
                heap.deref_ptr(vals).iter().for_each(|val| {
                    res += &val.repr(heap, symbol_table);
                    res += " ";
                });
                res += ")";
                res
            }
            NativeFunc(_) => std::string::String::from("Native function"),
            Func(func) => {
                let symbol = heap.deref_ptr(func).name;
                format!("Function {}", symbol_table[symbol])
            }
            Lambda(_) => std::string::String::from("Lambda object"),
            UserType(_) => "Not supported".into(),
        }
    }

    pub fn str(&self, heap: &Heap, symbol_table: &SymbolTable) -> std::string::String {
        use WeakVal::*;
        match self {
            NumberVal(val) => val.to_string(),
            StringVal(val) => heap.deref_ptr(val).clone(),
            Symbol(val) => symbol_table[*val].clone(),
            list @ List(_) => list.repr(heap, symbol_table),
            NativeFunc(_) => "Native function".to_owned(),
            Func(func) => {
                let name = &symbol_table[heap.deref_ptr(func).name];
                format!("Runtime function {}", name)
            }
            Lambda(_) => "Lambda function".to_owned(),
            _ => "Not supported".into(),
        }
    }

    pub fn simple_repr(&self, heap: &Heap) -> std::string::String {
        use WeakVal::*;
        match self {
            NumberVal(val) => val.to_string(),
            StringVal(val) => heap.deref_ptr(val).clone(),
            Symbol(val) => val.to_string(),
            List(vals) => {
                let mut res = std::string::String::from("(");
                heap.deref_ptr(vals).iter().for_each(|val| {
                    res += &val.simple_repr(heap);
                    res += " ";
                });
                res += ")";
                res
            }
            NativeFunc(_) => "Native function".to_owned(),
            Func(func) => {
                let name = heap.deref_ptr(func).name;
                format!("Runtime function {}", name)
            }
            Lambda(_) => "Lambda function".to_owned(),
            _ => "Not supported".into(),
        }
    }
}
