use std::collections::HashMap;
use std::convert::TryInto;

use crate::check_ptr;
use crate::data::{BuiltinSymbols, Environment, SymbolId, SymbolTable};
use crate::gc::{Heap, HeapMarked, MarkSweep, ScopedRef};
use crate::runtime::{drop_rooted_vec, RootedVal, WeakVal};
use crate::utils::JoinedIterator;

type Env = Environment;

/// Saved call stack
pub type SavedCtx = Vec<FuncFrame>;

#[derive(Clone)]
pub struct FuncFrame {
    pub globals: Env,
    pub locals: Option<Env>,
}

pub enum ModuleState {
    Evaluated(Env),
    Evaluating,
}

pub struct Interpreter {
    pub heap: Heap,
    pub gc: MarkSweep,
    pub call_stack: Vec<FuncFrame>,
    pub symbols: SymbolTable,
    pub modules: HashMap<String, ModuleState>,
}

impl Interpreter {
    pub fn new(
        heap: Heap,
        gc: MarkSweep,
        globals: Env,
        locals: Option<Env>,
        symbols: SymbolTable,
        main_module_name: &str,
    ) -> Self {
        let mut modules = HashMap::new();
        modules.insert(main_module_name.into(), ModuleState::Evaluating);
        Self {
            heap,
            gc,
            symbols,
            modules,
            call_stack: vec![FuncFrame { globals, locals }],
        }
    }

    pub fn eval(&mut self, expr: &RootedVal) -> RootedVal {
        self.eval_rooted(expr)
    }

    fn eval_rooted(&mut self, expr: &RootedVal) -> RootedVal {
        match expr {
            RootedVal::NumberVal(_) | RootedVal::StringVal(_) => expr.clone(&mut self.heap),
            RootedVal::Symbol(val) => self.eval_symbol(*val),
            RootedVal::List(val) => self.eval_list(val),
            _ => panic!("Cannot evaluate this node"),
        }
    }

    fn eval_weak(&mut self, expr: &WeakVal) -> RootedVal {
        match expr {
            WeakVal::NumberVal(_) | WeakVal::StringVal(_) => expr.clone().upgrade(&mut self.heap),
            WeakVal::Symbol(val) => self.eval_symbol(*val),
            WeakVal::List(val) => self.eval_list(val),
            _ => panic!("Cannot evaluate this node"),
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

    fn eval_list<Ptr>(&mut self, vals: &Ptr) -> RootedVal
    where
        Ptr: ScopedRef<Vec<WeakVal>>,
    {
        if self.heap.deref_ptr(vals).is_empty() {
            return RootedVal::nil(&mut self.heap);
        }
        if let Ok(val) = self.try_eval_special_form(vals) {
            return val;
        }
        let size = self.heap.deref_ptr(vals).len();
        let mut evaled = Vec::new();
        for i in 0..size {
            let raw_value = {
                let ptr_ref = &self.heap.deref_ptr(vals)[i];
                ptr_ref.clone()
            };
            let evaled_value = self.eval_weak(&raw_value);
            evaled.push(evaled_value);
        }
        let func = evaled.remove(0);
        let res = self.call(&func, evaled);
        func.heap_drop(&mut self.heap);
        res
    }

    pub fn call(&mut self, func: &RootedVal, args: Vec<RootedVal>) -> RootedVal {
        // println!("Calling func...");
        let res = match func {
            RootedVal::Func(func) => {
                check_ptr!(self.heap, func);
                let (func_body, func_args, func_globals) = {
                    let func = self.heap.deref_ptr(func);
                    (func.body.clone(), func.args.clone(), func.globals.clone())
                };
                let func_env = if func_args.is_empty() {
                    None
                } else {
                    Some(func_call_env(&mut self.heap, &func_args, args))
                };
                self.with_frame(
                    FuncFrame {
                        globals: func_globals,
                        locals: func_env,
                    },
                    |vm| vm.eval_weak(&func_body),
                )
            }
            RootedVal::Lambda(lambda_ref) => {
                check_ptr!(self.heap, lambda_ref);
                let (func_body, func_args, func_env, func_globals) = {
                    let func = self.heap.deref_ptr(lambda_ref);
                    (
                        func.body.clone(),
                        func.args.clone(),
                        func.env.clone(),
                        func.globals.clone(),
                    )
                };
                let func_env =
                    func_call_env_with_parent(&mut self.heap, &func_args, args, func_env);
                // TODO: DIFF WITH PREVIOUS VERSION TO SEE IF LOCALS WHERE CHANGED CORRECTLY
                // TODO: PUSH NEW GLOBALS CONTEXT
                self.with_frame(
                    FuncFrame {
                        globals: func_globals,
                        locals: Some(func_env),
                    },
                    |vm| vm.eval_weak(&func_body),
                )
            }
            RootedVal::NativeFunc(func) => {
                // println!("Native func...");
                let res = self.with_frame(
                    FuncFrame {
                        globals: self.get_globals(),
                        locals: None,
                    },
                    |vm| func(vm, args),
                );
                // println!("Returning from native");
                res
            }
            _ => panic!("first symbol of a list should refer to a function"),
        };
        // println!("\n------------ Running gc --------------");
        self.run_gc();
        // println!("-----------Gc step ended -------------\n");
        res
    }

    fn with_frame<R>(&mut self, frame: FuncFrame, func: impl FnOnce(&mut Self) -> R) -> R {
        self.call_stack.push(frame);
        let res = func(self);
        self.call_stack.pop();
        res
    }

    fn try_eval_special_form<Ptr>(&mut self, vals: &Ptr) -> Result<RootedVal, NotSpecialForm>
    where
        Ptr: ScopedRef<Vec<WeakVal>>,
    {
        let symbol = match &self.heap.deref_ptr(vals)[0] {
            WeakVal::Symbol(val) => *val,
            _ => return Err(NotSpecialForm),
        };
        let symbol = symbol.try_into();
        match symbol {
            Ok(BuiltinSymbols::Define) => Ok(self.eval_define(vals)),
            Ok(BuiltinSymbols::Begin) => Ok(self.eval_begin(vals)),
            Ok(BuiltinSymbols::Quote) => Ok(self.eval_quote(vals)),
            Ok(BuiltinSymbols::Quasiquote) => Ok(self.eval_quasiquote(vals)),
            Ok(BuiltinSymbols::Unquote) => panic!("Unquote in not quasiquoted context"),
            Ok(BuiltinSymbols::Lambda) => Ok(self.eval_lambda(vals)),
            Ok(BuiltinSymbols::If) => Ok(self.eval_if(vals)),
            Ok(BuiltinSymbols::While) => Ok(self.eval_while(vals)),
            _ => Err(NotSpecialForm),
        }
    }

    fn eval_define<Ptr>(&mut self, vals: &Ptr) -> RootedVal
    where
        Ptr: ScopedRef<Vec<WeakVal>>,
    {
        enum Pattern {
            Value {
                name: SymbolId,
                val: WeakVal,
            },
            Function {
                name: SymbolId,
                args: Vec<usize>,
                body: Vec<WeakVal>,
            },
        }

        let mut create_env = match self.get_locals() {
            None => self.get_globals(),
            Some(val) => val.clone(),
        };
        let constr = {
            let vals_ref = self.heap.deref_ptr(vals);
            match &vals_ref[..] {
                // variable define form
                [_, WeakVal::Symbol(name), val] => Pattern::Value {
                    name: *name,
                    val: val.clone(),
                },
                // function define form
                [_, WeakVal::List(inner), body @ ..] => {
                    if let [WeakVal::Symbol(name), args @ ..] = &self.heap.deref_ptr(inner)[..] {
                        Pattern::Function {
                            name: *name,
                            args: collect_function_definition_arguments(args),
                            body: body.iter().cloned().collect(),
                        }
                    } else {
                        panic!("wrong function define form")
                    }
                }
                _ => panic!("not a define form"),
            }
        };
        match constr {
            Pattern::Value { name, ref val } => {
                let evaled = self.eval_weak(val);
                create_env
                    .borrow_mut()
                    .values
                    .insert(name.clone(), evaled.downgrade(&mut self.heap));
            }
            Pattern::Function { name, args, body } => {
                let body = prepare_function_body(body, &mut self.heap);
                let globals = self.get_globals();
                let function = RootedVal::function(
                    name,
                    args,
                    body.downgrade(&mut self.heap),
                    globals,
                    &mut self.heap,
                );
                create_env
                    .borrow_mut()
                    .values
                    .insert(name.clone(), function.downgrade(&mut self.heap));
            }
        }
        RootedVal::none()
    }

    fn eval_lambda<Ptr>(&mut self, vals: &Ptr) -> RootedVal
    where
        Ptr: ScopedRef<Vec<WeakVal>>,
    {
        let locals = self
            .get_locals()
            .map(Environment::split)
            .unwrap_or_else(Environment::new);
        let globals = self.get_globals();
        let (args, body) = match &self.heap.deref_ptr(vals)[..] {
            [_, WeakVal::List(args), body @ ..] => {
                let args = collect_function_definition_arguments(&self.heap.deref_ptr(args));
                (args, body.iter().cloned().collect())
            }
            _ => panic!("not a lambda form"),
        };
        let body = prepare_function_body(body, &mut self.heap);
        RootedVal::lambda(
            locals,
            args,
            body.downgrade(&mut self.heap),
            globals,
            &mut self.heap,
        )
    }

    fn eval_begin<Ptr>(&mut self, vals: &Ptr) -> RootedVal
    where
        Ptr: ScopedRef<Vec<WeakVal>>,
    {
        let mut allocs = Vec::new();
        let size = self.heap.deref_ptr(vals).len();
        for i in 1..size {
            let raw_value = {
                let inner_ref = &self.heap.deref_ptr(vals)[i];
                inner_ref.clone()
            };
            allocs.push(self.eval_weak(&raw_value));
        }
        let res = allocs.pop().unwrap();
        drop_rooted_vec(&mut self.heap, allocs);
        res
    }

    fn eval_quote<Ptr>(&mut self, vals: &Ptr) -> RootedVal
    where
        Ptr: ScopedRef<Vec<WeakVal>>,
    {
        assert_eq!(
            self.heap.deref_ptr(vals).len(),
            2,
            "you can only quote single expression"
        );
        let quoted = self.heap.deref_ptr(vals)[1].clone();
        // Quote is just passing unevaluated code
        // the only thing we need to do is root it.
        quoted.upgrade(&mut self.heap)
    }

    fn eval_quasiquote<Ptr>(&mut self, vals: &Ptr) -> RootedVal
    where
        Ptr: ScopedRef<Vec<WeakVal>>,
    {
        assert_eq!(
            self.heap.deref_ptr(vals).len(),
            2,
            "you can only quote single expression"
        );
        let expr = self.heap.deref_ptr(vals)[1].clone();
        self.quasiquote_expr(&expr)
    }

    fn quasiquote_expr(&mut self, expr: &WeakVal) -> RootedVal {
        enum Action {
            Upgrade,
            Unquote,
            Recurse,
        }
        let action = match expr {
            WeakVal::Symbol(_) | WeakVal::NumberVal(_) | WeakVal::StringVal(_) => Action::Upgrade,
            WeakVal::List(inner) if self.heap.deref_ptr(inner).len() == 2 => {
                let head = &self.heap.deref_ptr(inner)[0];
                if head.is_symbol(BuiltinSymbols::Unquote as usize) {
                    Action::Unquote
                } else {
                    Action::Recurse
                }
            }
            WeakVal::List(_) => Action::Recurse,
            _ => panic!("couldn't possibly quasiquote this"),
        };
        match action {
            Action::Upgrade => expr.clone().upgrade(&mut self.heap),
            Action::Unquote => {
                if let WeakVal::List(inner) = expr {
                    let raw_val = {
                        let weak_ref = &self.heap.deref_ptr(inner)[1];
                        weak_ref.clone()
                    };
                    self.eval_weak(&raw_val)
                } else {
                    unreachable!("we checked before the shape of the expr")
                }
            }
            Action::Recurse => {
                if let WeakVal::List(inner) = expr {
                    let res = map_scoped_vec(self, inner, |vm, expr| vm.quasiquote_expr(expr));
                    RootedVal::list_from_rooted(res, &mut self.heap)
                } else {
                    unreachable!("we checked before the shape of the expr")
                }
            }
        }
    }

    fn eval_if<Ptr>(&mut self, expr: &Ptr) -> RootedVal
    where
        Ptr: ScopedRef<Vec<WeakVal>>,
    {
        assert_eq!(
            self.heap.deref_ptr(expr).len(),
            4,
            "If has to have condition and two clauses"
        );
        let (predicate, if_true, if_false) = {
            let predicate = self.heap.deref_ptr(expr)[1].clone();
            let if_true = self.heap.deref_ptr(expr)[2].clone();
            let if_false = self.heap.deref_ptr(expr)[3].clone();
            (predicate, if_true, if_false)
        };
        let res = self.eval_weak(&predicate);
        let eval_next = match res {
            RootedVal::Symbol(val) if val == BuiltinSymbols::True as SymbolId => if_true,
            RootedVal::Symbol(val) if val == BuiltinSymbols::False as SymbolId => if_false,
            _ => panic!("If predicate needs to eval to #t or #f"),
        };
        res.heap_drop(&mut self.heap);
        self.eval_weak(&eval_next)
    }

    fn eval_while<Ptr>(&mut self, expr: &Ptr) -> RootedVal
    where
        Ptr: ScopedRef<Vec<WeakVal>>,
    {
        assert!(self.heap.deref_ptr(expr).len() > 2, "while expr needs at least 2 clauses");
        let cond = self.heap.deref_ptr(expr)[1].clone();
        let size = self.heap.deref_ptr(expr).len();
        while {
            let cond_value = self.eval_weak(&cond);
            let next_it = cond_value.is_symbol(BuiltinSymbols::True as usize);
            cond_value.heap_drop(&mut self.heap);
            next_it
        } {
            for i in 2..size {
                let code = self.heap.deref_ptr(expr)[i].clone();
                let res = self.eval_weak(&code);
                res.heap_drop(&mut self.heap);
            }
        }
        RootedVal::none()
    }

    pub fn get_globals(&self) -> Env {
        // todo: do match instead of unwrap because
        // unwrap generates a lot od stack unwinding code
        self.call_stack.last().unwrap().globals.clone()
    }

    pub fn get_locals(&self) -> Option<Env> {
        // todo: do match instead of unwrap because
        // unwrap generates a lot od stack unwinding code
        self.call_stack.last().unwrap().locals.clone()
    }
    pub fn run_gc(&mut self) {
        self.gc
            .step(&mut self.heap, &mut self.call_stack, &mut self.modules);
    }

    pub fn get_value(&mut self, name: &str) -> Option<RootedVal> {
        let symbol = self.symbols.iter().position(|val| val == name)?;
        // println!("Got symbol {}", symbol);
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
        for (name, val) in (args, values).zip() {
            func_env_map.insert(name.clone(), val.downgrade(heap));
        }
    }
    env
}

struct NotSpecialForm;

fn collect_function_definition_arguments(args: &[WeakVal]) -> Vec<SymbolId> {
    args.iter()
        .map(|x| {
            if let WeakVal::Symbol(name) = x {
                name.clone()
            } else {
                panic!("function arguments should be symbols");
            }
        })
        .collect()
}

fn prepare_function_body(mut body: Vec<WeakVal>, heap: &mut Heap) -> RootedVal {
    assert!(!body.is_empty(), "function body cannot be empty");
    if body.len() == 1 {
        body.pop().unwrap().upgrade(heap)
    } else {
        let mut inner = vec![WeakVal::Symbol(BuiltinSymbols::Begin as SymbolId)];
        let iter = body.into_iter().map(|x| x.clone());
        inner.extend(iter);
        RootedVal::list(inner, heap)
    }
}

fn map_scoped_vec_range<Ptr, Func, Res>(
    vm: &mut Interpreter,
    ptr: &Ptr,
    mut func: Func,
    range: (usize, usize),
) -> Vec<Res>
where
    Ptr: ScopedRef<Vec<WeakVal>>,
    Func: FnMut(&mut Interpreter, &WeakVal) -> Res,
{
    let mut res = Vec::new();
    let size = vm.heap.deref_ptr(ptr).len();
    for i in range.0..(size - range.1) {
        let raw_val = {
            let val = &vm.heap.deref_ptr(ptr)[i];
            val.clone()
        };
        res.push(func(vm, &raw_val));
    }
    res
}

fn map_scoped_vec<Ptr, Func, Res>(vm: &mut Interpreter, ptr: &Ptr, func: Func) -> Vec<Res>
where
    Ptr: ScopedRef<Vec<WeakVal>>,
    Func: FnMut(&mut Interpreter, &WeakVal) -> Res,
{
    map_scoped_vec_range(vm, ptr, func, (0, 0))
}
