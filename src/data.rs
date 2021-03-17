use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    rc::Rc,
};

pub type NativeFunc = Rc<dyn Fn(Environment, Vec<RuntimeVal>) -> RuntimeVal>;

pub struct RuntimeFunc {
    pub body: Box<SExpr>,
    pub name: String,
    pub args: Vec<String>,
}

impl RuntimeFunc {
    fn new<Impl>(name: String, args: Vec<String>, body: SExpr) -> Rc<RuntimeFunc>
    where
        Impl: 'static + Fn(Environment) -> RuntimeVal,
    {
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
    Symbol(String),
    List(Vec<RuntimeVal>),
    Func(Rc<RuntimeFunc>),
    NativeFunc(NativeFunc),
}

impl RuntimeVal {
    pub fn nil() -> RuntimeVal {
        RuntimeVal::List(Vec::new())
    }

    pub fn native_function<Func>(func: Func) -> RuntimeVal
    where
        Func: 'static + Fn(Environment, Vec<RuntimeVal>) -> RuntimeVal,
    {
        RuntimeVal::NativeFunc(Rc::new(func))
    }

    pub fn repr(&self) -> String {
        match self {
            RuntimeVal::NumberVal(val) => val.to_string(),
            RuntimeVal::StringVal(val) => format!(r#""{}""#, val),
            RuntimeVal::Symbol(val) => val.clone(),
            RuntimeVal::List(vals) => {
                let mut res = String::from("(");
                vals.iter().for_each(|val| res += &val.repr());
                res += ")";
                res
            }
            RuntimeVal::NativeFunc(_) => String::from("Native function object"),
            RuntimeVal::Func(_) => String::from("Function object"),
        }
    }
}

pub enum SExpr {
    LitNumber(f64),
    LitString(String),
    Symbol(String),
    List(Vec<SExpr>),
}

pub struct EnvironmentImpl {
    pub parent: Option<Environment>,
    pub values: HashMap<String, RuntimeVal>,
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

    pub fn with_parent(parent: Environment) -> Self {
        Self::wrap(EnvironmentImpl::with_parent(parent))
    }

    pub fn borrow(&self) -> Ref<EnvironmentImpl> {
        self.0.borrow()
    }

    pub fn borrow_mut(&mut self) -> RefMut<EnvironmentImpl> {
        self.0.borrow_mut()
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
