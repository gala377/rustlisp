use core::panic;
use std::{iter::Enumerate, rc::Rc};

use crate::data::{Environment, RuntimeVal};
use crate::data::{EnvironmentImpl, SExpr};

pub fn eval(env: Environment, expr: &SExpr) -> RuntimeVal {
    match expr {
        SExpr::LitNumber(val) => RuntimeVal::NumberVal(*val),
        SExpr::LitString(val) => RuntimeVal::StringVal(val.clone()),
        SExpr::Symbol(val) => eval_symbol(env, val),
        SExpr::List(val) => eval_list(env, val),
    }
}

fn eval_symbol(env: Environment, expr: &String) -> RuntimeVal {
    env.borrow()
        .values
        .get(expr)
        .expect("symbol not defined")
        .clone()
}

fn eval_list(mut env: Environment, vals: &Vec<SExpr>) -> RuntimeVal {
    if vals.is_empty() {
        return RuntimeVal::nil();
    }
    if let Ok(val) = try_eval_special_form(env.clone(), vals) {
        return val;
    }
    let mut evaled: Vec<RuntimeVal> = vals.iter().map(|expr| eval(env.clone(), expr)).collect();
    let func = evaled.remove(0);
    match func {
        RuntimeVal::Func(func) => {
            let func_env = Environment::with_parent(env.clone());
            assert_eq!(evaled.len(), func.args.len());
            let func_env_map = &mut env.borrow_mut().values;
            for (name, val) in func.args.iter().zip(evaled.into_iter()) {
                func_env_map.insert(name.clone(), val);
            }
            eval(func_env, &func.body)
        }
        RuntimeVal::NativeFunc(func) => func(env, evaled),
        _ => panic!("first symbol of a list should refer to a function"),
    }
}

struct NotSpecialForm;

fn try_eval_special_form(
    env: Environment,
    vals: &Vec<SExpr>,
) -> Result<RuntimeVal, NotSpecialForm> {
    match &vals[0] {
        SExpr::Symbol(symbol) => match symbol.as_ref() {
            "def" => Ok(eval_define(env, vals)),
            _ => Err(NotSpecialForm),
        },
        _ => Err(NotSpecialForm),
    }
}

fn eval_define(mut env: Environment, vals: &Vec<SExpr>) -> RuntimeVal {
    if let [_, SExpr::List(inner), val] = vals.as_slice() {
        if let [SExpr::Symbol(name)] = inner.as_slice() {
            let evaled = eval(env.clone(), val);
            env.borrow_mut().values.insert(name.clone(), evaled);
            return RuntimeVal::nil();
        }
    }
    panic!("not a define form");
}
