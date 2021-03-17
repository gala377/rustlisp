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
    match func {
        RuntimeVal::Func(func) => {
            let mut func_env = Environment::with_parent(env.clone());
            assert_eq!(evaled.len(), func.args.len());
            let func_env_map = &mut Rc::get_mut(&mut func_env)
                .expect("func_env was cloned before")
                .values;
            for (name, val) in func.args.iter().zip(evaled.into_iter()) {
                func_env_map.insert(name.clone(), val);
            }
            eval(func_env, &func.body)
        }
        RuntimeVal::NativeFunc(func) => func(env, evaled),
        _ => panic!("first symbol of a list should refer to a function"),
    }
}
