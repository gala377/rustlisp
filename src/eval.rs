use std::convert::TryInto;

use crate::data::{BuiltinSymbols, Environment, RuntimeVal, SExpr, SymbolId, SymbolTable};

type Env = Environment;

// There are no lambdas yet.
// There is a problem with dynamic scoping which we do not want.
// When  we call a function we should only create a new env with global env as a parent
//

pub fn eval(
    globals: Env,
    locals: Option<Env>,
    symbols: &mut SymbolTable,
    expr: &SExpr,
) -> RuntimeVal {
    match expr {
        SExpr::LitNumber(val) => RuntimeVal::NumberVal(*val),
        SExpr::LitString(val) => RuntimeVal::StringVal(val.clone()),
        SExpr::Symbol(val) => eval_symbol(globals, locals, symbols, *val),
        SExpr::List(val) => eval_list(globals, locals, symbols, val),
    }
}

fn eval_symbol(
    globals: Env,
    locals: Option<Env>,
    symbols: &mut SymbolTable,
    expr: SymbolId,
) -> RuntimeVal {
    if let Some(mut env) = locals {
        loop {
            if let Some(val) = env.borrow().values.get(&expr) {
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
        .get(&expr)
        .expect(&format!("symbol {} not defined", symbols[expr]))
        .clone()
}

fn eval_list(
    globals: Env,
    locals: Option<Env>,
    symbols: &mut SymbolTable,
    vals: &Vec<SExpr>,
) -> RuntimeVal {
    if vals.is_empty() {
        return RuntimeVal::nil();
    }
    if let Ok(val) = try_eval_special_form(globals.clone(), locals.clone(), symbols, vals) {
        return val;
    }
    let mut evaled: Vec<RuntimeVal> = vals
        .iter()
        .map(|expr| eval(globals.clone(), locals.clone(), symbols, expr))
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
            eval(globals, Some(func_env), symbols, &func.body)
        }
        RuntimeVal::NativeFunc(func) => func(globals, symbols, evaled),
        _ => panic!("first symbol of a list should refer to a function"),
    }
}

pub fn call(
    globals: Env,
    symbols: &mut SymbolTable,
    func: &RuntimeVal,
    args: Vec<RuntimeVal>,
) -> RuntimeVal {
    match func {
        RuntimeVal::Func(func) => {
            let mut func_env = Environment::new();
            assert_eq!(args.len(), func.args.len());
            {
                let func_env_map = &mut func_env.borrow_mut().values;
                for (name, val) in func.args.iter().zip(args.into_iter()) {
                    func_env_map.insert(name.clone(), val);
                }
            }
            eval(globals, Some(func_env), symbols, &func.body)
        }
        RuntimeVal::NativeFunc(func) => func(globals, symbols, args),
        _ => panic!("first symbol of a list should refer to a function"),
    }
}

struct NotSpecialForm;

fn try_eval_special_form(
    globals: Env,
    locals: Option<Env>,
    symbols: &mut SymbolTable,
    vals: &Vec<SExpr>,
) -> Result<RuntimeVal, NotSpecialForm> {
    match &vals[0] {
        SExpr::Symbol(symbol) => match (*symbol).try_into() {
            Ok(BuiltinSymbols::Define) => Ok(eval_define(globals, locals, symbols, vals)),
            Ok(BuiltinSymbols::Begin) => Ok(eval_begin(globals, locals, symbols, vals)),
            Ok(BuiltinSymbols::Quote) => Ok(eval_quote(globals, locals, vals)),
            Ok(BuiltinSymbols::Quasiquote) => Ok(eval_quasiquote(globals, locals, symbols, vals)),
            Ok(BuiltinSymbols::Unquote) => panic!("Unquote in not quasiquoted context"),
            _ => Err(NotSpecialForm),
        },
        _ => Err(NotSpecialForm),
    }
}

fn eval_define(
    globals: Env,
    locals: Option<Env>,
    symbols: &mut SymbolTable,
    vals: &Vec<SExpr>,
) -> RuntimeVal {
    let mut create_env = match locals {
        None => globals.clone(),
        Some(ref val) => val.clone(),
    };
    match &vals[..] {
        // variable define form
        [_, SExpr::Symbol(name), val] => {
            let evaled = eval(globals.clone(), locals.clone(), symbols, val);
            create_env.borrow_mut().values.insert(name.clone(), evaled);
            return RuntimeVal::nil();
        }
        // function define form
        [_, SExpr::List(inner), body @ ..] => {
            if let [SExpr::Symbol(name), args @ ..] = &inner[..] {
                assert!(!body.is_empty(), "function body cannot be empty");
                let args: Vec<SymbolId> = args
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
                    let mut inner = vec![SExpr::Symbol(BuiltinSymbols::Begin as SymbolId)];
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

fn eval_begin(
    globals: Env,
    locals: Option<Env>,
    symbols: &mut SymbolTable,
    vals: &Vec<SExpr>,
) -> RuntimeVal {
    let mut res = RuntimeVal::nil();
    for expr in &vals[1..] {
        res = eval(globals.clone(), locals.clone(), symbols, expr);
    }
    res
}

fn eval_quote(_: Env, _: Option<Env>, vals: &Vec<SExpr>) -> RuntimeVal {
    assert_eq!(vals.len(), 2, "you can only quote single expression");
    quote_expr(&vals[1])
}

fn quote_expr(expr: &SExpr) -> RuntimeVal {
    match expr {
        SExpr::Symbol(val) => RuntimeVal::Symbol(val.clone()),
        SExpr::List(inner) => RuntimeVal::List(inner.iter().map(quote_expr).collect()),
        SExpr::LitNumber(val) => RuntimeVal::NumberVal(*val),
        SExpr::LitString(val) => RuntimeVal::StringVal(val.clone()),
    }
}

fn eval_quasiquote(
    globals: Env,
    locals: Option<Env>,
    symbols: &mut SymbolTable,
    vals: &Vec<SExpr>,
) -> RuntimeVal {
    assert_eq!(vals.len(), 2, "you can only quote single expression");
    quasiquote_expr(globals, locals, symbols, &vals[1])
}

fn quasiquote_expr(
    globals: Env,
    locals: Option<Env>,
    symbols: &mut SymbolTable,
    expr: &SExpr,
) -> RuntimeVal {
    match expr {
        SExpr::Symbol(val) => RuntimeVal::Symbol(val.clone()),
        SExpr::LitNumber(val) => RuntimeVal::NumberVal(*val),
        SExpr::LitString(val) => RuntimeVal::StringVal(val.clone()),
        SExpr::List(inner) if inner.len() > 0 => {
            if let SExpr::Symbol(val) = &inner[0] {
                if *val == BuiltinSymbols::Unquote as SymbolId {
                    assert_eq!(inner.len(), 2, "You can only unquote single expression");
                    return eval(globals, locals, symbols, &inner[1]);
                }
            }
            RuntimeVal::List(
                inner
                    .iter()
                    .map(|val| quasiquote_expr(globals.clone(), locals.clone(), symbols, val))
                    .collect(),
            )
        }
        SExpr::List(_) => RuntimeVal::List(Vec::new()),
    }
}
