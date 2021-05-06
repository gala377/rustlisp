use core::panic;
use std::convert::TryInto;

use crate::data::{BuiltinSymbols, Environment, SExpr, SymbolId, SymbolTable};
use crate::gc::Heap;
use crate::runtime::RuntimeVal;

type Env = Environment;

// todo:
// There are no lambdas yet.
// There is a problem with dynamic scoping which we do not want.
// When  we call a function we should only create a new env with global env as a parent

pub fn eval(
    heap: &mut Heap,
    globals: Env,
    locals: Option<Env>,
    symbols: &mut SymbolTable,
    expr: &SExpr,
) -> RuntimeVal {
    match expr {
        SExpr::LitNumber(val) => RuntimeVal::NumberVal(*val),
        SExpr::LitString(val) => RuntimeVal::string(val.clone(), heap),
        SExpr::Symbol(val) => eval_symbol(globals, locals, symbols, *val),
        SExpr::List(val) => eval_list(heap, globals, locals, symbols, val),
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
    heap: &mut Heap,
    globals: Env,
    locals: Option<Env>,
    symbols: &mut SymbolTable,
    vals: &Vec<SExpr>,
) -> RuntimeVal {
    if vals.is_empty() {
        return RuntimeVal::nil(heap);
    }
    if let Ok(val) = try_eval_special_form(heap, globals.clone(), locals.clone(), symbols, vals) {
        return val;
    }
    let mut evaled: Vec<RuntimeVal> = vals
        .iter()
        .map(|expr| eval(heap, globals.clone(), locals.clone(), symbols, expr))
        .collect();
    let func = evaled.remove(0);
    call(heap, globals, symbols, &func, evaled)
}

pub fn call(
    heap: &mut Heap,
    globals: Env,
    symbols: &mut SymbolTable,
    func: &RuntimeVal,
    args: Vec<RuntimeVal>,
) -> RuntimeVal {
    match func {
        RuntimeVal::Func(func) => {
            let func = unsafe { func.data.as_ref() };
            let func_env = func_call_env(&func.args, args);
            eval(heap, globals, Some(func_env), symbols, &func.body)
        }
        RuntimeVal::Lambda(lambda_ref) => {
            // lambda can mutably change its environment (not now but at some point)
            // Q: the same would apply for normal function and changing global env?
            // A: Yes, however normal function does not carry the globals inside its structure
            //    while lambda does carry its env with it. So normal function doesn't change itself
            //    while lambda can.
            let mut lambda_ref = lambda_ref.clone();
            let lambda = unsafe {
                // Note that this is safe. The lambda is immutable reference and by doing
                // clone we can borrow as mutable because of inner mutability.
                // This is fine because we modify runtime values on the heap and no
                // internal rust interpreter structs.
                lambda_ref.data.as_mut()
            };
            let func_env = func_call_env_with_parent(&lambda.args, args, lambda.env.clone());
            eval(heap, globals, Some(func_env), symbols, &lambda.body)
        }
        RuntimeVal::NativeFunc(func) => func(heap, globals, symbols, args),
        _ => panic!("first symbol of a list should refer to a function"),
    }
}

fn func_call_env(args: &[SymbolId], values: Vec<RuntimeVal>) -> Environment {
    let func_env = Environment::new();
    populate_env(func_env, args, values)
}

fn func_call_env_with_parent(
    args: &[SymbolId],
    values: Vec<RuntimeVal>,
    parent: Environment,
) -> Environment {
    let func_env = Environment::with_parent(parent);
    populate_env(func_env, args, values)
}

fn populate_env(mut env: Environment, args: &[SymbolId], values: Vec<RuntimeVal>) -> Environment {
    assert_eq!(values.len(), args.len());
    {
        let func_env_map = &mut env.borrow_mut().values;
        for (name, val) in args.iter().zip(values.into_iter()) {
            func_env_map.insert(name.clone(), val);
        }
    }
    env
}

struct NotSpecialForm;

fn try_eval_special_form(
    heap: &mut Heap,
    globals: Env,
    locals: Option<Env>,
    symbols: &mut SymbolTable,
    vals: &Vec<SExpr>,
) -> Result<RuntimeVal, NotSpecialForm> {
    match &vals[0] {
        SExpr::Symbol(symbol) => match (*symbol).try_into() {
            Ok(BuiltinSymbols::Define) => Ok(eval_define(heap, globals, locals, symbols, vals)),
            Ok(BuiltinSymbols::Begin) => Ok(eval_begin(heap, globals, locals, symbols, vals)),
            Ok(BuiltinSymbols::Quote) => Ok(eval_quote(heap, globals, locals, vals)),
            Ok(BuiltinSymbols::Quasiquote) => {
                Ok(eval_quasiquote(heap, globals, locals, symbols, vals))
            }
            Ok(BuiltinSymbols::Unquote) => panic!("Unquote in not quasiquoted context"),
            Ok(BuiltinSymbols::Lambda) => Ok(eval_lambda(heap, globals, locals, symbols, vals)),
            _ => Err(NotSpecialForm),
        },
        _ => Err(NotSpecialForm),
    }
}

fn eval_define(
    heap: &mut Heap,
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
            let evaled = eval(heap, globals.clone(), locals.clone(), symbols, val);
            create_env.borrow_mut().values.insert(name.clone(), evaled);
            RuntimeVal::nil(heap)
        }
        // function define form
        [_, SExpr::List(inner), body @ ..] => {
            if let [SExpr::Symbol(name), args @ ..] = &inner[..] {
                let body = prepare_function_body(body);
                let args = collect_function_definition_arguments(args);
                let function = RuntimeVal::function(name.clone(), args, body, heap);
                create_env
                    .borrow_mut()
                    .values
                    .insert(name.clone(), function);
                RuntimeVal::nil(heap)
            } else {
                panic!("wrong function define form")
            }
        }
        _ => panic!("not a define form"),
    }
}

fn eval_lambda(
    heap: &mut Heap,
    _globals: Env,
    locals: Option<Env>,
    _symbols: &mut SymbolTable,
    vals: &Vec<SExpr>,
) -> RuntimeVal {
    let locals = locals.map(Environment::split).unwrap_or(Environment::new());
    match &vals[..] {
        [_, SExpr::List(args), body @ ..] => {
            let body = prepare_function_body(body);
            let args = collect_function_definition_arguments(args);
            RuntimeVal::lambda(locals, args, body, heap)
        }
        _ => panic!("not a lambda form"),
    }
}

fn collect_function_definition_arguments(args: &[SExpr]) -> Vec<SymbolId> {
    args.iter()
        .map(|x| {
            if let SExpr::Symbol(name) = x {
                name.clone()
            } else {
                panic!("function arguments should be symbols");
            }
        })
        .collect()
}

fn prepare_function_body(body: &[SExpr]) -> SExpr {
    assert!(!body.is_empty(), "function body cannot be empty");
    if body.len() == 1 {
        body[0].clone()
    } else {
        let mut inner = vec![SExpr::Symbol(BuiltinSymbols::Begin as SymbolId)];
        inner.extend(body.into_iter().map(Clone::clone));
        SExpr::List(inner)
    }
}

fn eval_begin(
    heap: &mut Heap,
    globals: Env,
    locals: Option<Env>,
    symbols: &mut SymbolTable,
    vals: &Vec<SExpr>,
) -> RuntimeVal {
    let mut res = RuntimeVal::nil(heap);
    for expr in &vals[1..] {
        res = eval(heap, globals.clone(), locals.clone(), symbols, expr);
    }
    res
}

fn eval_quote(heap: &mut Heap, _: Env, _: Option<Env>, vals: &Vec<SExpr>) -> RuntimeVal {
    assert_eq!(vals.len(), 2, "you can only quote single expression");
    quote_expr(heap, &vals[1])
}

fn quote_expr(heap: &mut Heap, expr: &SExpr) -> RuntimeVal {
    match expr {
        SExpr::Symbol(val) => RuntimeVal::Symbol(val.clone()),
        SExpr::List(inner) => {
            RuntimeVal::list(inner.iter().map(|x| quote_expr(heap, x)).collect(), heap)
        }
        SExpr::LitNumber(val) => RuntimeVal::NumberVal(*val),
        SExpr::LitString(val) => RuntimeVal::string(val.clone(), heap),
    }
}

fn eval_quasiquote(
    heap: &mut Heap,
    globals: Env,
    locals: Option<Env>,
    symbols: &mut SymbolTable,
    vals: &Vec<SExpr>,
) -> RuntimeVal {
    assert_eq!(vals.len(), 2, "you can only quote single expression");
    quasiquote_expr(heap, globals, locals, symbols, &vals[1])
}

fn quasiquote_expr(
    heap: &mut Heap,
    globals: Env,
    locals: Option<Env>,
    symbols: &mut SymbolTable,
    expr: &SExpr,
) -> RuntimeVal {
    match expr {
        SExpr::Symbol(val) => RuntimeVal::Symbol(val.clone()),
        SExpr::LitNumber(val) => RuntimeVal::NumberVal(*val),
        SExpr::LitString(val) => RuntimeVal::string(val.clone(), heap),
        SExpr::List(inner) if inner.len() > 0 => {
            if let SExpr::Symbol(val) = &inner[0] {
                if *val == BuiltinSymbols::Unquote as SymbolId {
                    assert_eq!(inner.len(), 2, "You can only unquote single expression");
                    return eval(heap, globals, locals, symbols, &inner[1]);
                }
            }
            RuntimeVal::list(
                inner
                    .iter()
                    .map(|val| quasiquote_expr(heap, globals.clone(), locals.clone(), symbols, val))
                    .collect(),
                heap,
            )
        }
        SExpr::List(_) => RuntimeVal::nil(heap),
    }
}
