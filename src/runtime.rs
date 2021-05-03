use crate::data::{Environment, RuntimeVal, SExpr, SymbolId};

pub struct Lambda {
    pub body: Box<SExpr>,
    pub args: Vec<SymbolId>,
    pub env: Environment,
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
