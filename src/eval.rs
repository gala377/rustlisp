use core::panic;

use crate::data::{Environment, RuntimeVal, SExpr};


type Env = Environment;

// There are no lambdas yet.
// There is a problem with dynamic scoping which we do not want.
// When  we call a function we should only create a new env with global env as a parent
//

pub fn eval(globals: Env, locals: Option<Env>, expr: &SExpr) -> RuntimeVal {
    match expr {
        SExpr::LitNumber(val) => RuntimeVal::NumberVal(*val),
        SExpr::LitString(val) => RuntimeVal::StringVal(val.clone()),
        SExpr::Symbol(val) => eval_symbol(globals, locals, val),
        SExpr::List(val) => eval_list(globals, locals, val),
    }
}

fn eval_symbol(globals: Env, locals: Option<Env>, expr: &String) -> RuntimeVal {
    if let Some(mut env) = locals {
        loop {
            if let Some(val) = env.borrow().values.get(expr) {
                return val.clone();
            }
            if let Some(val) = env.into_parent() {
                env = val;
            } else {
                break;
            }
        }
    }
    globals
        .borrow()
        .values
        .get(expr)
        .expect(&format!("symbol {} not defined", expr))
        .clone()
}

fn eval_list(globals: Env, locals: Option<Env>, vals: &Vec<SExpr>) -> RuntimeVal {
    if vals.is_empty() {
        return RuntimeVal::nil();
    }
    if let Ok(val) = try_eval_special_form(globals.clone(), locals.clone(), vals) {
        return val;
    }
    let mut evaled: Vec<RuntimeVal> = vals
        .iter()
        .map(|expr| eval(globals.clone(), locals.clone(), expr))
        .collect();
    let func = evaled.remove(0);
    match func {
        RuntimeVal::Func(func) => {
            let mut func_env = Environment::new();
            assert_eq!(evaled.len(), func.args.len());
            {
                let func_env_map = &mut func_env.borrow_mut().values;
                for (name, val) in func.args.iter().zip(evaled.into_iter()) {
                    func_env_map.insert(name.clone(), val);
                }
            }
            eval(globals, Some(func_env), &func.body)
        }
        RuntimeVal::NativeFunc(func) => func(globals, evaled),
        _ => panic!("first symbol of a list should refer to a function"),
    }
}

struct NotSpecialForm;

fn try_eval_special_form(
    globals: Env,
    locals: Option<Env>,
    vals: &Vec<SExpr>,
) -> Result<RuntimeVal, NotSpecialForm> {
    match &vals[0] {
        SExpr::Symbol(symbol) => match symbol.as_ref() {
            "def" => Ok(eval_define(globals, locals, vals)),
            "begin" => Ok(eval_begin(globals, locals, vals)),
            _ => Err(NotSpecialForm),
        },
        _ => Err(NotSpecialForm),
    }
}

fn eval_define(globals: Env, locals: Option<Env>, vals: &Vec<SExpr>) -> RuntimeVal {
    let mut create_env = match locals {
        None => globals.clone(),
        Some(ref val) => val.clone(),
    };
    match &vals[..] {
        // variable define form
        [_, SExpr::Symbol(name), val] => {
            let evaled = eval(globals.clone(), locals.clone(), val);
            create_env.borrow_mut().values.insert(name.clone(), evaled);
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
                create_env
                    .borrow_mut()
                    .values
                    .insert(name.clone(), function);
                return RuntimeVal::nil();
            } else {
                panic!("wrong function define form");
            }
        }
        _ => (),
    }
    panic!("not a define form");
}

fn eval_begin(globals: Env, locals: Option<Env>, vals: &Vec<SExpr>) -> RuntimeVal {
    let mut res = RuntimeVal::nil();
    for expr in &vals[1..] {
        res = eval(globals.clone(), locals.clone(), expr);
    }
    res
}
