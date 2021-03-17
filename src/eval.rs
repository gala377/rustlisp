use core::panic;

use crate::data::{Environment, RuntimeVal, SExpr};

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
            "begin" => Ok(eval_begin(env, vals)),
            _ => Err(NotSpecialForm),
        },
        _ => Err(NotSpecialForm),
    }
}

fn eval_define(mut env: Environment, vals: &Vec<SExpr>) -> RuntimeVal {
    match &vals[..] {
        // variable define form
        [_, SExpr::Symbol(name), val] => {
            let evaled = eval(env.clone(), val);
            env.borrow_mut().values.insert(name.clone(), evaled);
            return RuntimeVal::nil();
        }
        // function define form
        [_, SExpr::List(inner), body @ ..] => {
            if let [SExpr::Symbol(name), args @ ..] = &inner[..] {
                assert!(!body.is_empty(), "function body cannot be empty");
                let args: Vec<String> = args
                    .iter()
                    .map(|x| {
                        if let SExpr::Symbol(name) = x {
                            name.clone()
                        } else {
                            panic!("function arguments should be symbols");
                        }
                    })
                    .collect();
                let body = if body.len() == 1 {
                    body[0].clone()
                } else {
                    let mut inner = vec![SExpr::Symbol(String::from("begin"))];
                    inner.extend(body.into_iter().map(Clone::clone));
                    SExpr::List(inner)
                };
                let function = RuntimeVal::function(name.clone(), args, body);
                env.borrow_mut().values.insert(name.clone(), function);
                return RuntimeVal::nil();
            } else {
                panic!("wrong function define form");
            }
        }
        _ => (),
    }
    panic!("not a define form");
}

fn eval_begin(env: Environment, vals: &Vec<SExpr>) -> RuntimeVal {
    let mut res = RuntimeVal::nil();
    for expr in &vals[1..] {
        res = eval(env.clone(), expr);
    }
    res
}
