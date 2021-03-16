use std::{collections::HashMap, rc::Rc};

pub type RuntimeFunc = Box<dyn Fn(Rc<Environment>, Vec<RuntimeVal>) -> RuntimeVal>;

pub enum RuntimeVal {
    NumberVal(f64),
    StringVal(String),
    Symbol(String),
    List(Vec<RuntimeVal>),
    Func(RuntimeFunc),
}

impl RuntimeVal {

    pub fn nil() -> RuntimeVal {
        RuntimeVal::List(Vec::new())
    }

    pub fn function<Func>(func: Func) -> RuntimeVal
    where Func: 'static + Fn(Rc<Environment>, Vec<RuntimeVal>) -> RuntimeVal
    {
        RuntimeVal::Func(Box::new(func))
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

pub struct Environment {
    pub parent: Option<Rc<Environment>>,
    pub values: HashMap<String, RuntimeVal>,
}

impl Environment {
    pub fn new() -> Rc<Environment> {
        Rc::new(Self {
            parent: None,
            values: HashMap::new(),
        })
    }

    pub fn with_parent(parent: Rc<Environment>) -> Rc<Environment> {
        Rc::new(Self {
            parent: Some(parent),
            values: HashMap::new(),
        })
    }
}

