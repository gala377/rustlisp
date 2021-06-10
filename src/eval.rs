use core::panic;
use std::convert::TryInto;

use crate::data::{BuiltinSymbols, Environment, SExpr, SymbolId, SymbolTable};
use crate::gc::{Heap, MarkSweep};
use crate::runtime::{drop_rooted_vec, RootedVal};
use crate::utils::print_sexpr_impl;

type Env = Environment;




pub fn eval(
    heap: &mut Heap,
    gc: &mut MarkSweep,
    globals: Env,
    locals: Option<Env>,
    symbols: &mut SymbolTable,
    expr: &SExpr,
) -> RootedVal {
    match expr {
        SExpr::LitNumber(val) => RootedVal::NumberVal(*val),
        SExpr::LitString(val) => RootedVal::string(val.clone(), heap),
        SExpr::Symbol(val) => eval_symbol(heap, globals, locals, symbols, *val),
        SExpr::List(val) => eval_list(heap, gc, globals, locals, symbols, val),
    }
}

fn eval_symbol(
    heap: &mut Heap,
    globals: Env,
    locals: Option<Env>,
    symbols: &mut SymbolTable,
    expr: SymbolId,
) -> RootedVal {
    if let Some(mut env) = locals {
        loop {
            if let Some(val) = env.borrow().values.get(&expr) {
                return val.clone().upgrade(heap);
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
        .upgrade(heap)
}

fn eval_list(
    heap: &mut Heap,
    gc: &mut MarkSweep,
    globals: Env,
    locals: Option<Env>,
    symbols: &mut SymbolTable,
    vals: &Vec<SExpr>,
) -> RootedVal {
    if vals.is_empty() {
        return RootedVal::nil(heap);
    }
    if let Ok(val) = try_eval_special_form(heap, gc, globals.clone(), locals.clone(), symbols, vals)
    {
        return val;
    }
    let mut evaled: Vec<RootedVal> = vals
        .iter()
        .map(|expr| eval(heap, gc, globals.clone(), locals.clone(), symbols, expr))
        .collect();
    let func = evaled.remove(0);
    let res = call(heap, gc, globals, symbols, &func, evaled);
    func.heap_drop(heap);
    res
}

pub fn call(
    heap: &mut Heap,
    gc: &mut MarkSweep,
    globals: Env,
    symbols: &mut SymbolTable,
    func: &RootedVal,
    args: Vec<RootedVal>,
) -> RootedVal {
    let globals_copy = globals.clone();
    let res = match func {
        RootedVal::Func(func) => unsafe {
            let ptr = func.data.get();
            let func = ptr.as_ref();
            let func_env = func_call_env(heap, &func.args, args);
            eval(heap, gc, globals, Some(func_env), symbols, &func.body)
        },
        RootedVal::Lambda(lambda_ref) => unsafe {
            let ptr = lambda_ref.data.get();
            let lambda = ptr.as_ref();
            let func_env = func_call_env_with_parent(heap, &lambda.args, args, lambda.env.clone());
            eval(heap, gc, globals, Some(func_env), symbols, &lambda.body)
        },
        RootedVal::NativeFunc(func) => func(heap, gc, globals, symbols, args),
        _ => panic!("first symbol of a list should refer to a function"),
    };
    println!("\n------------ Running gc --------------");
    gc.step(heap, globals_copy, None);
    println!("-----------Gc step ended -------------\n");
    res
}

fn func_call_env(heap: &mut Heap, args: &[SymbolId], values: Vec<RootedVal>) -> Environment {
    let func_env = Environment::new();
    populate_env(heap, func_env, args, values)
}

fn func_call_env_with_parent(
    heap: &mut Heap,
    args: &[SymbolId],
    values: Vec<RootedVal>,
    parent: Environment,
) -> Environment {
    let func_env = Environment::with_parent(parent);
    populate_env(heap, func_env, args, values)
}

fn populate_env(
    heap: &mut Heap,
    mut env: Environment,
    args: &[SymbolId],
    values: Vec<RootedVal>,
) -> Environment {
    assert_eq!(values.len(), args.len());
    {
        let func_env_map = &mut env.borrow_mut().values;
        for (name, val) in args.iter().zip(values.into_iter()) {
            func_env_map.insert(name.clone(), val.downgrade(heap));
        }
    }
    env
}

struct NotSpecialForm;

fn try_eval_special_form(
    heap: &mut Heap,
    gc: &mut MarkSweep,
    globals: Env,
    locals: Option<Env>,
    symbols: &mut SymbolTable,
    vals: &Vec<SExpr>,
) -> Result<RootedVal, NotSpecialForm> {
    match &vals[0] {
        SExpr::Symbol(symbol) => match (*symbol).try_into() {
            Ok(BuiltinSymbols::Define) => Ok(eval_define(heap, gc, globals, locals, symbols, vals)),
            Ok(BuiltinSymbols::Begin) => Ok(eval_begin(heap, gc, globals, locals, symbols, vals)),
            Ok(BuiltinSymbols::Quote) => Ok(eval_quote(heap, globals, locals, vals)),
            Ok(BuiltinSymbols::Quasiquote) => {
                Ok(eval_quasiquote(heap, gc, globals, locals, symbols, vals))
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
    gc: &mut MarkSweep,
    globals: Env,
    locals: Option<Env>,
    symbols: &mut SymbolTable,
    vals: &Vec<SExpr>,
) -> RootedVal {
    let mut create_env = match locals {
        None => globals.clone(),
        Some(ref val) => val.clone(),
    };
    match &vals[..] {
        // variable define form
        [_, SExpr::Symbol(name), val] => {
            let evaled = eval(heap, gc, globals.clone(), locals.clone(), symbols, val);
            create_env
                .borrow_mut()
                .values
                .insert(name.clone(), evaled.downgrade(heap));
            RootedVal::nil(heap)
        }
        // function define form
        [_, SExpr::List(inner), body @ ..] => {
            if let [SExpr::Symbol(name), args @ ..] = &inner[..] {
                let body = prepare_function_body(body);
                let args = collect_function_definition_arguments(args);
                let function = RootedVal::function(name.clone(), args, body, heap);
                create_env
                    .borrow_mut()
                    .values
                    .insert(name.clone(), function.downgrade(heap));
                RootedVal::nil(heap)
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
) -> RootedVal {
    let locals = locals.map(Environment::split).unwrap_or(Environment::new());
    match &vals[..] {
        [_, SExpr::List(args), body @ ..] => {
            let body = prepare_function_body(body);
            let args = collect_function_definition_arguments(args);
            RootedVal::lambda(locals, args, body, heap)
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
    gc: &mut MarkSweep,
    globals: Env,
    locals: Option<Env>,
    symbols: &mut SymbolTable,
    vals: &Vec<SExpr>,
) -> RootedVal {
    let mut allocs = Vec::new();
    for expr in &vals[1..] {
        allocs.push(eval(
            heap,
            gc,
            globals.clone(),
            locals.clone(),
            symbols,
            expr,
        ));
    }
    let res = allocs.pop().unwrap();
    drop_rooted_vec(heap, allocs);
    res
}

fn eval_quote(heap: &mut Heap, _: Env, _: Option<Env>, vals: &Vec<SExpr>) -> RootedVal {
    assert_eq!(vals.len(), 2, "you can only quote single expression");
    quote_expr(heap, &vals[1])
}

fn quote_expr(heap: &mut Heap, expr: &SExpr) -> RootedVal {
    match expr {
        SExpr::Symbol(val) => RootedVal::Symbol(val.clone()),
        SExpr::List(inner) => {
            RootedVal::list_from_rooted(inner.iter().map(|x| quote_expr(heap, x)).collect(), heap)
        }
        SExpr::LitNumber(val) => RootedVal::NumberVal(*val),
        SExpr::LitString(val) => RootedVal::string(val.clone(), heap),
    }
}

fn eval_quasiquote(
    heap: &mut Heap,
    gc: &mut MarkSweep,
    globals: Env,
    locals: Option<Env>,
    symbols: &mut SymbolTable,
    vals: &Vec<SExpr>,
) -> RootedVal {
    assert_eq!(vals.len(), 2, "you can only quote single expression");
    quasiquote_expr(heap, gc, globals, locals, symbols, &vals[1])
}

fn quasiquote_expr(
    heap: &mut Heap,
    gc: &mut MarkSweep,
    globals: Env,
    locals: Option<Env>,
    symbols: &mut SymbolTable,
    expr: &SExpr,
) -> RootedVal {
    match expr {
        SExpr::Symbol(val) => RootedVal::Symbol(val.clone()),
        SExpr::LitNumber(val) => RootedVal::NumberVal(*val),
        SExpr::LitString(val) => RootedVal::string(val.clone(), heap),
        SExpr::List(inner) if inner.len() > 0 => {
            if let SExpr::Symbol(val) = &inner[0] {
                if *val == BuiltinSymbols::Unquote as SymbolId {
                    assert_eq!(inner.len(), 2, "You can only unquote single expression");
                    return eval(heap, gc, globals, locals, symbols, &inner[1]);
                }
            }
            RootedVal::list_from_rooted(
                inner
                    .iter()
                    .map(|val| {
                        quasiquote_expr(heap, gc, globals.clone(), locals.clone(), symbols, val)
                    })
                    .collect(),
                heap,
            )
        }
        SExpr::List(_) => RootedVal::nil(heap),
    }
}
