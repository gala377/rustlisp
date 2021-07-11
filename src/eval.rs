use core::panic;
use std::convert::TryInto;
use std::ops::Index;

use crate::check_ptr;
use crate::data::{BuiltinSymbols, Environment, SExpr, SymbolId, SymbolTable};
use crate::gc::{Heap, MarkSweep};
use crate::runtime::{drop_rooted_vec, RootedVal};
use crate::utils::print_sexpr_impl;

type Env = Environment;

/// Tuple of saved globals and locals
pub type SavedCtx = (Env, Vec<Env>);

pub struct Interpreter {
    pub heap: Heap,
    pub gc: MarkSweep,
    pub globals: Env,
    pub locals: Vec<Env>,
    pub symbols: SymbolTable,
    pub saved_ctxs: Vec<SavedCtx>,
}

impl Interpreter {
    pub fn new(
        heap: Heap,
        gc: MarkSweep,
        globals: Env,
        locals: Option<Env>,
        symbols: SymbolTable,
    ) -> Self {
        Self {
            heap,
            gc,
            globals: globals,
            locals: locals.into_iter().collect(),
            symbols,
            saved_ctxs: Vec::new(),
        }
    }

    pub fn eval(&mut self, expr: &SExpr) -> RootedVal {
        match expr {
            SExpr::LitNumber(val) => RootedVal::NumberVal(*val),
            // TODO: Allocate trough method instead of constructor
            SExpr::LitString(val) => RootedVal::string(val.clone(), &mut self.heap),
            SExpr::Symbol(val) => self.eval_symbol(*val),
            SExpr::List(val) => self.eval_list(val),
        }
    }

    fn eval_symbol(&mut self, expr: SymbolId) -> RootedVal {
        if let Some(env) = self.get_locals() {
            let mut env = env.clone();
            loop {
                if let Some(val) = env.borrow().values.get(&expr) {
                    return val.clone().upgrade(&mut self.heap);
                }
                if let Some(val) = env.into_parent() {
                    env = val;
                } else {
                    break;
                }
            }
        }
        self.get_globals()
            .borrow()
            .values
            .get(&expr)
            .expect(&format!("symbol {} not defined", self.symbols[expr]))
            .clone()
            .upgrade(&mut self.heap)
    }

    fn eval_list(&mut self, vals: &Vec<SExpr>) -> RootedVal {
        if vals.is_empty() {
            return RootedVal::nil(&mut self.heap);
        }
        if let Ok(val) = self.try_eval_special_form(vals) {
            return val;
        }
        let mut evaled: Vec<RootedVal> = vals.iter().map(|expr| self.eval(expr)).collect();
        let func = evaled.remove(0);
        let res = self.call(&func, evaled);
        func.heap_drop(&mut self.heap);
        res
    }

    pub fn call(&mut self, func: &RootedVal, args: Vec<RootedVal>) -> RootedVal {
        println!("Calling func...");
        let res = match func {
            RootedVal::Func(func) => unsafe {
                check_ptr!(self.heap, func);
                let func = func.data.as_ref();
                let func_env = func_call_env(&mut self.heap, &func.args, args);
                self.with_locals(func_env, |vm| vm.eval(&func.body))
            },
            RootedVal::Lambda(lambda_ref) => unsafe {
                check_ptr!(self.heap, lambda_ref);
                let lambda = lambda_ref.data.as_ref();
                let func_env = func_call_env_with_parent(
                    &mut self.heap,
                    &lambda.args,
                    args,
                    lambda.env.clone(),
                );
                // TODO: DIFF WITH PREVIOUS VERSION TO SEE IF LOCALS WHERE CHANGED CORRECTLY
                self.with_locals(func_env, |vm| vm.eval(&lambda.body))
            },
            RootedVal::NativeFunc(func) => {
                println!("Native func...");
                let res = self.with_locals(Env::new(), |vm| func(vm, args));

                println!("Returning from native");
                res
            }
            _ => panic!("first symbol of a list should refer to a function"),
        };
        println!("\n------------ Running gc --------------");
        self.gc.step(
            &mut self.heap,
            &self.globals,
            &self.locals,
            &self.saved_ctxs,
        );
        println!("-----------Gc step ended -------------\n");
        res
    }

    fn with_locals<R>(&mut self, env: Env, func: impl FnOnce(&mut Self) -> R) -> R {
        self.locals.push(env);
        let res = func(self);
        self.locals.pop();
        res
    }

    fn try_eval_special_form(&mut self, vals: &Vec<SExpr>) -> Result<RootedVal, NotSpecialForm> {
        match &vals[0] {
            SExpr::Symbol(symbol) => match (*symbol).try_into() {
                Ok(BuiltinSymbols::Define) => Ok(self.eval_define(vals)),
                Ok(BuiltinSymbols::Begin) => Ok(self.eval_begin(vals)),
                Ok(BuiltinSymbols::Quote) => Ok(self.eval_quote(vals)),
                Ok(BuiltinSymbols::Quasiquote) => Ok(self.eval_quasiquote(vals)),
                Ok(BuiltinSymbols::Unquote) => panic!("Unquote in not quasiquoted context"),
                Ok(BuiltinSymbols::Lambda) => Ok(self.eval_lambda(vals)),
                _ => Err(NotSpecialForm),
            },
            _ => Err(NotSpecialForm),
        }
    }

    fn eval_define(&mut self, vals: &Vec<SExpr>) -> RootedVal {
        let mut create_env = match self.get_locals() {
            None => self.get_globals().clone(),
            Some(val) => val.clone(),
        };
        match &vals[..] {
            // variable define form
            [_, SExpr::Symbol(name), val] => {
                let evaled = self.eval(val);
                create_env
                    .borrow_mut()
                    .values
                    .insert(name.clone(), evaled.downgrade(&mut self.heap));
                RootedVal::nil(&mut self.heap)
            }
            // function define form
            [_, SExpr::List(inner), body @ ..] => {
                if let [SExpr::Symbol(name), args @ ..] = &inner[..] {
                    let body = prepare_function_body(body);
                    let args = collect_function_definition_arguments(args);
                    let function = RootedVal::function(name.clone(), args, body, &mut self.heap);
                    create_env
                        .borrow_mut()
                        .values
                        .insert(name.clone(), function.downgrade(&mut self.heap));
                    RootedVal::nil(&mut self.heap)
                } else {
                    panic!("wrong function define form")
                }
            }
            _ => panic!("not a define form"),
        }
    }

    fn eval_lambda(&mut self, vals: &Vec<SExpr>) -> RootedVal {
        let locals = self
            .get_locals()
            .map(Environment::split)
            .unwrap_or(Environment::new());
        match &vals[..] {
            [_, SExpr::List(args), body @ ..] => {
                let body = prepare_function_body(body);
                let args = collect_function_definition_arguments(args);
                RootedVal::lambda(locals, args, body, &mut self.heap)
            }
            _ => panic!("not a lambda form"),
        }
    }

    fn eval_begin(&mut self, vals: &Vec<SExpr>) -> RootedVal {
        let mut allocs = Vec::new();
        for expr in &vals[1..] {
            allocs.push(self.eval(expr));
        }
        let res = allocs.pop().unwrap();
        drop_rooted_vec(&mut self.heap, allocs);
        res
    }

    fn eval_quote(&mut self, vals: &Vec<SExpr>) -> RootedVal {
        assert_eq!(vals.len(), 2, "you can only quote single expression");
        self.quote_expr(&vals[1])
    }

    fn quote_expr(&mut self, expr: &SExpr) -> RootedVal {
        match expr {
            SExpr::Symbol(val) => RootedVal::Symbol(val.clone()),
            SExpr::List(inner) => RootedVal::list_from_rooted(
                inner.iter().map(|x| self.quote_expr(x)).collect(),
                &mut self.heap,
            ),
            SExpr::LitNumber(val) => RootedVal::NumberVal(*val),
            SExpr::LitString(val) => RootedVal::string(val.clone(), &mut self.heap),
        }
    }

    fn eval_quasiquote(&mut self, vals: &Vec<SExpr>) -> RootedVal {
        assert_eq!(vals.len(), 2, "you can only quote single expression");
        self.quasiquote_expr(&vals[1])
    }

    fn quasiquote_expr(&mut self, expr: &SExpr) -> RootedVal {
        match expr {
            SExpr::Symbol(val) => RootedVal::Symbol(val.clone()),
            SExpr::LitNumber(val) => RootedVal::NumberVal(*val),
            SExpr::LitString(val) => RootedVal::string(val.clone(), &mut self.heap),
            SExpr::List(inner) if inner.len() > 0 => {
                if let SExpr::Symbol(val) = &inner[0] {
                    if *val == BuiltinSymbols::Unquote as SymbolId {
                        assert_eq!(inner.len(), 2, "You can only unquote single expression");
                        return self.eval(&inner[1]);
                    }
                }
                RootedVal::list_from_rooted(
                    inner.iter().map(|val| self.quasiquote_expr(val)).collect(),
                    &mut self.heap,
                )
            }
            SExpr::List(_) => RootedVal::nil(&mut self.heap),
        }
    }

    pub fn get_globals(&self) -> Env {
        self.globals.clone()
    }

    pub fn get_locals(&self) -> Option<Env> {
        self.locals.last().cloned()
    }

    pub fn save_context(&mut self, ctx: SavedCtx) {
        // TODO: Remove this function and in place
        // put proper module handling
        self.saved_ctxs.insert(0, ctx);
    }

    pub fn push_context(&mut self, globals: Env, locals: Vec<Env>) {
        self.saved_ctxs
            .push((self.globals.clone(), self.locals.clone()));
        self.globals = globals;
        self.locals = locals;
    }

    pub fn pop_context(&mut self) -> SavedCtx {
        let ctx = self.saved_ctxs.pop().unwrap();
        let old_ctx = (self.globals.clone(), self.locals.clone());
        self.globals = ctx.0;
        self.locals = ctx.1;
        old_ctx
    }

    pub fn get_value(&mut self, name: &str) -> Option<RootedVal> {
        let symbol = self.symbols.iter().position(|val| val == name)?;
        println!("Got symbol {}", symbol);
        Some(self.eval_symbol(symbol))
    }
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
    assert_eq!(
        values.len(),
        args.len(),
        "Call provided more values than function args"
    );
    {
        let func_env_map = &mut env.borrow_mut().values;
        for (name, val) in args.iter().zip(values.into_iter()) {
            func_env_map.insert(name.clone(), val.downgrade(heap));
        }
    }
    env
}

struct NotSpecialForm;

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
