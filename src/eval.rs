use std::{collections::HashMap, convert::TryInto};

#[cfg(debug)]
use crate::gc::HeapMarked;
use crate::{env::{BuiltinSymbols, Environment, SymbolId, SymbolTable}, gc::{self, Heap, MarkSweep, ScopedMutPtr, ScopedPtr, ScopedRef}, runtime::{RootedVal, RuntimeFunc, WeakVal}, utils::JoinedIterator};

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
            RootedVal::NumberVal(_) | RootedVal::StringVal(_) => expr.clone(),
            RootedVal::Symbol(val) => self.eval_symbol(*val),
            RootedVal::List(val) => self.eval_list(val),
            _ => panic!("Cannot evaluate this node"),
        }
    }

    fn eval_weak(&mut self, expr: &WeakVal) -> RootedVal {
        match expr {
            WeakVal::NumberVal(_) | WeakVal::StringVal(_) => expr.as_root(),
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
                    return val.as_root();
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
            .upgrade()
    }

    fn eval_list<Ptr>(&mut self, vals: &Ptr) -> RootedVal
    where
        Ptr: ScopedRef<Vec<WeakVal>>,
    {
        if self.get_ref(vals).is_empty() {
            return RootedVal::nil(&mut self.heap);
        }
        if let Ok(val) = self.try_eval_special_form(vals) {
            return val;
        }
        let func = self.get_ref(vals)[0].clone();
        let func_evaled = self.eval_weak(&func);
        if let RootedVal::Macro(macro_body) = &func_evaled {
            let macro_args = map_scoped_vec_range(self, vals, (1,0), |_, val| val.as_root());
            return self.expand_macro(macro_body, macro_args);
        }
        let evaled = map_scoped_vec_range(self, vals, (1,0), |vm, val| vm.eval_weak(val));
        self.call(&func_evaled, evaled)
    }

    pub fn call(&mut self, func: &RootedVal, args: Vec<RootedVal>) -> RootedVal {
        let res = match func {
            RootedVal::Func(func) => {
                let (func_body, func_args, func_globals) = {
                    let func = self.get_ref(func);
                    (func.body.clone(), func.args.clone(), func.globals.clone())
                };
                let func_env = func_call_env(&func_args, args);
                self.with_frame(
                    FuncFrame {
                        globals: func_globals,
                        locals: Some(func_env),
                    },
                    |vm| vm.eval_weak(&func_body),
                )
            }
            RootedVal::Lambda(lambda_ref) => {
                let (func_body, func_args, func_env, func_globals) = {
                    let func = self.get_ref(lambda_ref);
                    (
                        func.body.clone(),
                        func.args.clone(),
                        func.env.clone(),
                        func.globals.clone(),
                    )
                };
                let func_env = func_call_env_with_parent(&func_args, args, func_env);
                self.with_frame(
                    FuncFrame {
                        globals: func_globals,
                        locals: Some(func_env),
                    },
                    |vm| vm.eval_weak(&func_body),
                )
            }
            RootedVal::NativeFunc(func) => self.with_frame(
                FuncFrame {
                    globals: self.get_globals(),
                    locals: None,
                },
                |vm| func(vm, args),
            ),
            _ => panic!("first symbol of a list should refer to a function"),
        };
        // If we've allocated more that 3/4th of the tracking entries
        if self.heap.taken_entries.get() > 3 * self.heap.vacant_entries.get() {
            self.run_gc();
        }
        res
    }

    fn expand_macro(&mut self, macro_body: &gc::Root<RuntimeFunc>, args: Vec<RootedVal>) -> RootedVal {
        let (func_body, func_args, func_globals) = {
            let func = self.get_ref(macro_body);
            (func.body.clone(), func.args.clone(), func.globals.clone())
        };
        let func_env = func_call_env(&func_args, args);
        let res = self.with_frame(
            FuncFrame {
                globals: func_globals,
                locals: Some(func_env),
            },
            |vm| vm.eval_weak(&func_body),
        );
        self.eval_rooted(&res)
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
        let symbol = match &self.get_ref(vals)[0] {
            WeakVal::Symbol(val) => *val,
            _ => return Err(NotSpecialForm),
        };
        match symbol.try_into() {
            Ok(BuiltinSymbols::Define) => Ok(self.eval_define(vals)),
            Ok(BuiltinSymbols::Begin) => Ok(self.eval_begin(vals)),
            Ok(BuiltinSymbols::Quote) => Ok(self.eval_quote(vals)),
            Ok(BuiltinSymbols::Quasiquote) => Ok(self.eval_quasiquote(vals)),
            Ok(BuiltinSymbols::Unquote) => panic!("Unquote in not quasiquoted context"),
            Ok(BuiltinSymbols::Lambda) => Ok(self.eval_lambda(vals)),
            Ok(BuiltinSymbols::If) => Ok(self.eval_if(vals)),
            Ok(BuiltinSymbols::While) => Ok(self.eval_while(vals)),
            Ok(BuiltinSymbols::Set) => Ok(self.eval_set(vals)),
            Ok(BuiltinSymbols::Let) => Ok(self.eval_let(vals)),
            Ok(BuiltinSymbols::Splice) => panic!("unquote-splicing is not quasiquote context"),
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
            let vals_ref = self.get_ref(vals);
            match &vals_ref[..] {
                // variable define form
                [_, WeakVal::Symbol(name), val] => Pattern::Value {
                    name: *name,
                    val: val.clone(),
                },
                // function define form
                [_, WeakVal::List(inner), body @ ..] => {
                    if let [WeakVal::Symbol(name), args @ ..] = &self.get_ref(inner)[..] {
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
                    .insert(name.clone(), evaled.downgrade());
            }
            Pattern::Function { name, args, body } => {
                let body = prepare_function_body(body, &mut self.heap);
                let globals = self.get_globals();
                let function =
                    RootedVal::function(name, args, body.downgrade(), globals, &mut self.heap);
                create_env
                    .borrow_mut()
                    .values
                    .insert(name.clone(), function.downgrade());
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
        let (args, body) = match &self.get_ref(vals)[..] {
            [_, WeakVal::List(args), body @ ..] => {
                let args = collect_function_definition_arguments(&self.get_ref(args));
                (args, body.iter().cloned().collect())
            }
            _ => panic!("not a lambda form"),
        };
        let body = prepare_function_body(body, &mut self.heap);
        RootedVal::lambda(locals, args, body.downgrade(), globals, &mut self.heap)
    }

    fn eval_begin<Ptr>(&mut self, vals: &Ptr) -> RootedVal
    where
        Ptr: ScopedRef<Vec<WeakVal>>,
    {
        map_scoped_vec_range(self, vals, (1, 0), |vm, val| vm.eval_weak(val))
            .pop()
            .unwrap()
    }

    fn eval_quote<Ptr>(&mut self, vals: &Ptr) -> RootedVal
    where
        Ptr: ScopedRef<Vec<WeakVal>>,
    {
        assert_eq!(
            self.get_ref(vals).len(),
            2,
            "you can only quote single expression"
        );
        // Quote is just passing unevaluated code
        // the only thing we need to do is root it.
        self.get_ref(vals)[1].as_root()
    }

    fn eval_quasiquote<Ptr>(&mut self, vals: &Ptr) -> RootedVal
    where
        Ptr: ScopedRef<Vec<WeakVal>>,
    {
        assert_eq!(
            self.get_ref(vals).len(),
            2,
            "you can only quasiquote single expression"
        );
        let expr = self.get_ref(vals)[1].clone();
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
            WeakVal::List(inner) if self.get_ref(inner).len() == 2 => {
                let head = &self.get_ref(inner)[0];
                if head.is_builtin_symbol(BuiltinSymbols::Unquote) {
                    Action::Unquote
                } else {
                    Action::Recurse
                }
            }
            WeakVal::List(_) => Action::Recurse,
            _ => panic!("couldn't possibly quasiquote this"),
        };
        match action {
            Action::Upgrade => expr.as_root(),
            Action::Unquote => {
                if let WeakVal::List(inner) = expr {
                    let raw_val = self.get_ref(inner)[1].clone();
                    self.eval_weak(&raw_val)
                } else {
                    unreachable!("we checked before the shape of the expr")
                }
            }
            Action::Recurse => {
                if let WeakVal::List(ptr) = expr {
                    let len = self.get_ref(ptr).len();
                    let mut res = Vec::new();
                    for i in 0..len {
                        let raw_val = self.get_ref(ptr)[i].clone();
                        match &raw_val {
                            WeakVal::List(inner) if self.get_ref(inner).len() == 2 => {
                                if !self.handle_possible_unquote_splicing(&mut res, inner) {
                                    res.push(self.quasiquote_expr(&raw_val));
                                }
                            }
                            _ => res.push(self.quasiquote_expr(&raw_val)),
                        }
                    }
                    RootedVal::list_from_rooted(res, &mut self.heap)
                } else {
                    unreachable!("we checked before the shape of the expr")
                }
            }
        }
    }

    fn handle_possible_unquote_splicing(
        &mut self,
        res: &mut Vec<RootedVal>,
        expr: &gc::Weak<Vec<WeakVal>>,
    ) -> bool {
        let head = &self.get_ref(expr)[0];
        if !head.is_builtin_symbol(BuiltinSymbols::Splice) {
            return false;
        }
        let rest = self.get_ref(expr)[1].clone();
        let rest_evaled = self.eval_weak(&rest);
        match &rest_evaled {
            RootedVal::List(inner) => {
                for val in &*self.get_ref(inner) {
                    res.push(val.as_root())
                }
            }
            _ => panic!("Unquote-splicing argument has to evaluate to a list"),
        }
        true
    }

    fn eval_if<Ptr>(&mut self, expr: &Ptr) -> RootedVal
    where
        Ptr: ScopedRef<Vec<WeakVal>>,
    {
        assert_eq!(
            self.get_ref(expr).len(),
            4,
            "If has to have condition and two clauses"
        );
        let (predicate, if_true, if_false) = {
            let predicate = self.get_ref(expr)[1].clone();
            let if_true = self.get_ref(expr)[2].clone();
            let if_false = self.get_ref(expr)[3].clone();
            (predicate, if_true, if_false)
        };
        let res = self.eval_weak(&predicate);
        let eval_next = match res {
            RootedVal::Symbol(val) if val == BuiltinSymbols::True as SymbolId => if_true,
            RootedVal::Symbol(val) if val == BuiltinSymbols::False as SymbolId => if_false,
            _ => panic!("If predicate needs to eval to #t or #f"),
        };
        self.eval_weak(&eval_next)
    }

    fn eval_while<Ptr>(&mut self, expr: &Ptr) -> RootedVal
    where
        Ptr: ScopedRef<Vec<WeakVal>>,
    {
        assert!(
            self.get_ref(expr).len() > 2,
            "while expr needs at least 2 clauses"
        );
        let cond = self.get_ref(expr)[1].clone();
        while self
            .eval_weak(&cond)
            .is_symbol(BuiltinSymbols::True as usize)
        {
            map_scoped_vec_range(self, expr, (2, 0), |vm, val| vm.eval_weak(val));
        }
        RootedVal::none()
    }

    fn eval_set<Ptr>(&mut self, expr: &Ptr) -> RootedVal
    where
        Ptr: ScopedRef<Vec<WeakVal>>,
    {
        assert_eq!(self.get_ref(expr).len(), 3, "set form takes 2 arguments");
        let location = self.get_ref(expr)[1].clone();
        let val = self.get_ref(expr)[2].clone();
        let val = self.eval_weak(&val);
        match &location {
            WeakVal::Symbol(inner) => {
                let mut found = false;
                if let Some(env) = self.get_locals() {
                    let mut env = env.clone();
                    found = loop {
                        if let Some(env_entry) = env.borrow_mut().values.get_mut(inner) {
                            let val = val.as_weak();
                            *env_entry = val;
                            break true;
                        };
                        if let Some(val) = env.into_parent() {
                            env = val;
                        } else {
                            break false;
                        }
                    };
                }
                if !found {
                    match self.get_globals().borrow_mut().values.get_mut(inner) {
                        Some(env_entry) => {
                            let val = val.as_weak();
                            *env_entry = val;
                        }
                        None => panic!("Trying to set! not defined location"),
                    }
                }
            }
            WeakVal::List(inner) => {
                let (list_expr, index_expr) = match &self.get_ref(inner)[..] {
                    [list_expr, index_expr] => (list_expr.clone(), index_expr.clone()),
                    _ => panic!("Invalid list set!"),
                };
                let (mut list, index) = {
                    let list = self.eval_weak(&list_expr);
                    let index = self.eval_weak(&index_expr);
                    match (list, index) {
                        (RootedVal::List(ptr), RootedVal::NumberVal(index)) => (ptr, index),
                        _ => panic!("List set! target types mismatched"),
                    }
                };
                let index = index as usize;
                let len = self.get_ref(&list).len();
                if index >= len {
                    panic!("Trying to set list location past its size")
                }
                let val = val.as_weak();
                self.get_mut(&mut list)[index] = val;
            }
            _ => panic!("Invalid set!"),
        }
        RootedVal::none()
    }

    fn eval_let<Ptr>(&mut self, expr: &Ptr) -> RootedVal
    where
        Ptr: ScopedRef<Vec<WeakVal>>,
    {
        let size = self.get_ref(expr).len();
        assert!(size > 2, "let form needs at least 2 arguments");
        let bindings = self.get_ref(expr)[1].clone();
        // prepare let env
        let mut evaled_bindings = HashMap::new();
        match &bindings {
            WeakVal::List(inner) => {
                let to_evaluate = map_scoped_vec(self, inner, |vm, binding| match binding {
                    WeakVal::List(binding) => match &vm.get_ref(binding)[..] {
                        [WeakVal::Symbol(name), body] => (*name, body.clone()),
                        _ => panic!("Wrong binding form, expected Symbol and an expression"),
                    },
                    _ => panic!("Each binding should be a list of length 2"),
                });
                for (name, body) in to_evaluate {
                    let body = self.eval_weak(&body);
                    evaled_bindings.insert(name, body);
                }
            }
            _ => panic!("Let bindings have to be a list"),
        };
        let let_env: HashMap<_, _> = evaled_bindings
            .into_iter()
            .map(|(name, val)| (name, val.downgrade()))
            .collect();
        let mut let_env: Environment = let_env.into();
        {
            let curr_frame = self.get_frame_mut();
            match &curr_frame.locals {
                None => curr_frame.locals = Some(let_env.clone()),
                Some(env) => {
                    let_env.reparent(env.clone());
                    curr_frame.locals = Some(let_env.clone())
                }
            }
        };
        // eval body
        let mut results = map_scoped_vec_range(self, expr, (2, 0), |vm, val| vm.eval_weak(val));
        let res = results
            .pop()
            .expect("Let evaluation should not yield empty results");
        // pop local env
        self.get_frame_mut().locals = let_env.into_parent();
        res
    }

    #[cfg(debug)]
    #[inline]
    pub fn get_ref<'a, T, Ptr>(&'a self, ptr: &'a Ptr) -> ScopedPtr<T>
    where
        Ptr: ScopedRef<T> + HeapMarked,
    {
        self.heap.deref_ptr(ptr)
    }

    #[cfg(not(debug))]
    #[inline]
    pub fn get_ref<'a, T>(&'a self, ptr: &'a impl ScopedRef<T>) -> ScopedPtr<T> {
        self.heap.deref_ptr(ptr)
    }

    #[cfg(debug)]
    #[inline]
    pub fn get_mut<'a, T, Ptr>(&'a mut self, ptr: &'a mut Ptr) -> ScopedMutPtr<T>
    where
        Ptr: ScopedRef<T> + HeapMarked,
    {
        self.heap.deref_ptr_mut(ptr)
    }

    #[cfg(not(debug))]
    #[inline]
    pub fn get_mut<'a, T>(&'a mut self, ptr: &'a mut impl ScopedRef<T>) -> ScopedMutPtr<T> {
        self.heap.deref_ptr_mut(ptr)
    }

    #[inline]
    pub fn get_frame(&self) -> &FuncFrame {
        self.call_stack.last().unwrap()
    }

    #[inline]
    pub fn get_frame_mut(&mut self) -> &mut FuncFrame {
        self.call_stack.last_mut().unwrap()
    }

    #[inline]
    pub fn get_globals(&self) -> Env {
        // todo: do match instead of unwrap because
        // unwrap generates a lot od stack unwinding code
        self.get_frame().globals.clone()
    }

    #[inline]
    pub fn get_locals(&self) -> Option<Env> {
        // todo: do match instead of unwrap because
        // unwrap generates a lot od stack unwinding code
        self.get_frame().locals.clone()
    }

    pub fn run_gc(&mut self) {
        self.gc
            .step(&mut self.heap, &mut self.call_stack, &mut self.modules);
    }

    pub fn get_value(&mut self, name: &str) -> Option<RootedVal> {
        let symbol = self.symbols.get(name)?;
        Some(self.eval_symbol(symbol))
    }
}

#[inline]
fn func_call_env(args: &[SymbolId], values: Vec<RootedVal>) -> Environment {
    let func_env = Environment::new();
    populate_env(func_env, args, values)
}

#[inline]
fn func_call_env_with_parent(
    args: &[SymbolId],
    values: Vec<RootedVal>,
    parent: Environment,
) -> Environment {
    let func_env = Environment::with_parent(parent);
    populate_env(func_env, args, values)
}

fn populate_env(mut env: Environment, args: &[SymbolId], values: Vec<RootedVal>) -> Environment {
    assert_eq!(
        values.len(),
        args.len(),
        "Call provided more values than function args"
    );
    {
        let func_env_map = &mut env.borrow_mut().values;
        for (name, val) in (args, values).zip() {
            func_env_map.insert(name.clone(), val.downgrade());
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
        body.pop().unwrap().upgrade()
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
    range: (usize, usize),
    mut func: Func,
) -> Vec<Res>
where
    Ptr: ScopedRef<Vec<WeakVal>>,
    Func: FnMut(&mut Interpreter, &WeakVal) -> Res,
{
    let len = vm.get_ref(ptr).len();
    let mut res = Vec::with_capacity(len - range.0 - range.1);
    let size = vm.get_ref(ptr).len();
    for i in range.0..(size - range.1) {
        let raw_val = {
            let val = &vm.get_ref(ptr)[i];
            val.clone()
        };
        res.push(func(vm, &raw_val));
    }
    res
}

#[inline]
fn map_scoped_vec<Ptr, Func, Res>(vm: &mut Interpreter, ptr: &Ptr, func: Func) -> Vec<Res>
where
    Ptr: ScopedRef<Vec<WeakVal>>,
    Func: FnMut(&mut Interpreter, &WeakVal) -> Res,
{
    map_scoped_vec_range(vm, ptr, (0, 0), func)
}
