use std::rc::Rc;

use crate::data::RuntimeVal;
use crate::data::{Environment, SExpr};

pub fn eval(env: Rc<Environment>, expr: &SExpr) -> RuntimeVal {
    match expr {
        SExpr::LitNumber(val) => RuntimeVal::NumberVal(*val),
        SExpr::LitString(val) => RuntimeVal::StringVal(val.clone()),
        SExpr::Symbol(val) => eval_symbol(env, val),
        SExpr::List(val) => eval_list(env, val),
    }
}

fn eval_symbol(env: Rc<Environment>, expr: &String) -> RuntimeVal {
    let val = env.values.get(expr).expect("symbol not defined");
    (*val).clone()
}

fn eval_list(env: Rc<Environment>, vals: &Vec<SExpr>) -> RuntimeVal {
    if vals.is_empty() {
        return RuntimeVal::nil();
    }
    let mut evaled: Vec<RuntimeVal> = vals.iter().map(|expr| eval(env.clone(), expr)).collect();
    let func = evaled.remove(0);
    if let RuntimeVal::Func(func) = func {
        func(env, evaled)
    } else {
        panic!("first symbol of a list should refer to a function");
    }
    // todo: how about you know, creating an environment for values passed
    // as arguments? Its different for native functions as they do not
    // have named arguments so for them vector is fine.
}
