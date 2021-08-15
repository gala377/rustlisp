use crate::{
    env::BuiltinSymbols,
    env::{Environment, SymbolId, SymbolTable},
    eval::Interpreter,
    gc::{self, Allocable, Heap, Root, TypeTag, Weak},
    native::{NativeStruct, RootedStructPtr, WeakStructPtr},
};
use std::{ops::Deref, rc::Rc};

#[derive(Clone)]
pub struct FunctionArgs {
    pub positional: Vec<SymbolId>,
    pub rest: Option<SymbolId>,
}

pub struct Lambda {
    pub body: WeakVal,
    pub args: FunctionArgs,
    pub env: Environment,
    pub globals: Environment,
}

impl Lambda {
    fn new(env: Environment, args: FunctionArgs, body: WeakVal, globals: Environment) -> Self {
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
    pub args: FunctionArgs,
    pub globals: Environment,
}

impl RuntimeFunc {
    fn new(name: SymbolId, args: FunctionArgs, body: WeakVal, globals: Environment) -> RuntimeFunc {
        Self {
            name,
            args,
            globals,
            body,
        }
    }
}

pub type NativeFunc = Rc<dyn Fn(&mut Interpreter, Vec<RootedVal>) -> RootedVal>;

impl Allocable for WeakVal {
    fn tag() -> TypeTag {
        TypeTag::Boxed
    }
}

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
    Symbol(SymbolId),

    // Reference immutable types
    Func(Root<RuntimeFunc>),
    Macro(Root<RuntimeFunc>),
    NativeFunc(NativeFunc),
    StringVal(Root<std::string::String>),

    // Mutable reference types
    Lambda(Root<Lambda>),
    List(Root<Vec<WeakVal>>),
    UserType(RootedStructPtr),
    Boxed {
        inner: Root<WeakVal>,
        auto_deref: bool,
    },
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
            Macro(x) => WeakVal::Macro(x.downgrade()),
            Boxed { inner, auto_deref } => WeakVal::Boxed {
                inner: inner.downgrade(),
                auto_deref,
            },
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
        args: FunctionArgs,
        body: WeakVal,
        globals: Environment,
        heap: &mut Heap,
    ) -> RootedVal {
        let inner = heap.allocate(RuntimeFunc::new(name, args, body, globals));
        Self::Func(inner)
    }

    pub fn macro_val(
        name: SymbolId,
        args: FunctionArgs,
        body: WeakVal,
        globals: Environment,
        heap: &mut Heap,
    ) -> RootedVal {
        let inner = heap.allocate(RuntimeFunc::new(name, args, body, globals));
        Self::Macro(inner)
    }

    pub fn lambda(
        env: Environment,
        args: FunctionArgs,
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
        let mut inner: Root<Vec<WeakVal>> = heap.allocate(Vec::with_capacity(val.len()));
        heap.deref_ptr_mut(&mut inner)
            .extend(val.into_iter().map(RootedVal::downgrade));
        Self::List(inner)
    }

    pub fn native_function<Func>(func: Func) -> RootedVal
    where
        Func: 'static + Fn(&mut Interpreter, Vec<RootedVal>) -> RootedVal,
    {
        RootedVal::NativeFunc(Rc::new(func))
    }

    pub fn boxed(val: WeakVal, heap: &mut Heap) -> RootedVal {
        let inner = heap.allocate(val);
        Self::Boxed {
            inner,
            auto_deref: false,
        }
    }

    pub fn boxed_unfrozen(val: WeakVal, heap: &mut Heap) -> RootedVal {
        let inner = heap.allocate(val);
        Self::Boxed {
            inner,
            auto_deref: true,
        }
    }

    pub fn unbox(&self, heap: &mut Heap) -> RootedVal {
        if let Self::Boxed { inner, .. } = self {
            let inner_ref = heap.deref_ptr(inner);
            inner_ref.as_root()
        } else {
            self.clone()
        }
    }

    pub fn set_box(&mut self, val: WeakVal, heap: &mut Heap) {
        if let Self::Boxed { inner, .. } = self {
            let mut inner_ref = heap.deref_ptr_mut(inner);
            *inner_ref = val;
        } else {
            panic!("Value is not a box")
        }
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
                if res.chars().last().unwrap().is_whitespace() {
                    res.pop();
                }
                res += ")";
                res
            }
            NativeFunc(_) => std::string::String::from("Native function"),
            Func(func) => {
                let symbol = heap.deref_ptr(func).name;
                format!("Function {}", symbol_table[symbol])
            }
            Macro(func) => {
                let symbol = heap.deref_ptr(func).name;
                format!("Macro {}", symbol_table[symbol])
            }
            Lambda(_) => std::string::String::from("Lambda object"),
            Boxed { inner, auto_deref } => {
                if *auto_deref {
                    format!("Boxed*[{}]", heap.deref_ptr(inner).repr(heap, symbol_table))
                } else {
                    format!("Boxed[{}]", heap.deref_ptr(inner).repr(heap, symbol_table))
                }
            }
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
    Macro(gc::Weak<RuntimeFunc>),
    NativeFunc(NativeFunc),
    Lambda(gc::Weak<Lambda>),
    UserType(WeakStructPtr),
    Boxed {
        inner: gc::Weak<WeakVal>,
        auto_deref: bool,
    },
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
            Macro(x) => RootedVal::Macro(x.upgrade()),
            Boxed { inner, auto_deref } => RootedVal::Boxed {
                inner: inner.upgrade(),
                auto_deref,
            },
        }
    }

    pub fn as_root(&self) -> RootedVal {
        self.clone().upgrade()
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

    pub fn is_builtin_symbol(&self, sym: BuiltinSymbols) -> bool {
        self.is_symbol(sym as usize)
    }

    pub fn unbox(&self, heap: &mut Heap) -> RootedVal {
        if let Self::Boxed { inner, .. } = self {
            let inner_ref = heap.deref_ptr(inner);
            inner_ref.as_root()
        } else {
            self.as_root()
        }
    }

    pub fn set_box(&mut self, val: WeakVal, heap: &mut Heap) {
        if let Self::Boxed { inner, .. } = self {
            let mut inner_ref = heap.deref_ptr_mut(inner);
            *inner_ref = val;
        } else {
            panic!("Value is not a box")
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
            Macro(func) => {
                let symbol = heap.deref_ptr(func).name;
                format!("Macro {}", symbol_table[symbol])
            }
            Lambda(_) => std::string::String::from("Lambda object"),
            UserType(_) => "Not supported".into(),
            Boxed { inner, auto_deref } => {
                if *auto_deref {
                    format!("Boxed*[{}]", heap.deref_ptr(inner).repr(heap, symbol_table))
                } else {
                    format!("Boxed[{}]", heap.deref_ptr(inner).repr(heap, symbol_table))
                }
            }
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
            Macro(func) => {
                let symbol = heap.deref_ptr(func).name;
                format!("Macro {}", symbol_table[symbol])
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
                if res.chars().last().unwrap().is_whitespace() {
                    res.pop();
                }
                res += ")";
                res
            }
            NativeFunc(_) => "Native function".to_owned(),
            Func(func) => {
                let name = heap.deref_ptr(func).name;
                format!("Runtime function {}", name)
            }
            Macro(func) => {
                let symbol = heap.deref_ptr(func).name;
                format!("Macro {}", symbol)
            }
            Lambda(_) => "Lambda function".to_owned(),
            _ => "Not supported".into(),
        }
    }
}
